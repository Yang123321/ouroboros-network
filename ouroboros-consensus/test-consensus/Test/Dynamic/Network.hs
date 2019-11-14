{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Setup network
module Test.Dynamic.Network (
    runNodeNetwork
  , TracingConstraints
    -- * Tracers
  , MiniProtocolExpectedException (..)
  , MiniProtocolState (..)
  , TraceMiniProtocolRestart (..)
    -- * Test Output
  , TestOutput (..)
  , NodeOutput (..)
  , NodeDBs (..)
  ) where

import qualified Control.Exception as Exn
import           Control.Monad
import           Control.Tracer
import           Crypto.Random (ChaChaDRG, drgNew)
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe, isJust)
import           Data.Proxy (Proxy (..))
import           Data.Set (Set)
import qualified Data.Set as Set
import           GHC.Stack

import qualified Control.Monad.Class.MonadAsync as Async
import           Control.Monad.Class.MonadThrow

import           Network.TypedProtocol.Channel
import           Network.TypedProtocol.Codec (AnyMessage (..))

import           Ouroboros.Network.Block
import           Ouroboros.Network.MockChain.Chain

import qualified Ouroboros.Network.BlockFetch.Client as BFClient
import           Ouroboros.Network.Protocol.BlockFetch.Type
import           Ouroboros.Network.Protocol.ChainSync.PipelineDecision
                     (pipelineDecisionLowHighMark)
import           Ouroboros.Network.Protocol.ChainSync.Type
import           Ouroboros.Network.Protocol.TxSubmission.Type
import qualified Ouroboros.Network.TxSubmission.Inbound as TxInbound
import qualified Ouroboros.Network.TxSubmission.Outbound as TxOutbound

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime
import qualified Ouroboros.Consensus.BlockFetchServer as BFServer
import           Ouroboros.Consensus.ChainSyncClient (ClockSkew (..))
import qualified Ouroboros.Consensus.ChainSyncClient as CSClient
import           Ouroboros.Consensus.ChainSyncServer (Tip)
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Mock
import           Ouroboros.Consensus.Mempool
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Node.Tracers
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.NodeKernel
import           Ouroboros.Consensus.NodeNetwork
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Consensus.Util.ResourceRegistry
import           Ouroboros.Consensus.Util.STM

import qualified Ouroboros.Storage.ChainDB as ChainDB
import           Ouroboros.Storage.ChainDB.Impl (ChainDbArgs (..))
import           Ouroboros.Storage.EpochInfo (EpochInfo, newEpochInfo)
import qualified Ouroboros.Storage.ImmutableDB as ImmDB
import qualified Ouroboros.Storage.LedgerDB.DiskPolicy as LgrDB
import qualified Ouroboros.Storage.LedgerDB.InMemory as LgrDB
import qualified Ouroboros.Storage.Util.ErrorHandling as EH

import           Test.Dynamic.TxGen
import           Test.Dynamic.Util.NodeJoinPlan
import           Test.Dynamic.Util.NodeTopology

import           Test.Util.FS.Sim.MockFS (MockFS)
import qualified Test.Util.FS.Sim.MockFS as Mock
import           Test.Util.FS.Sim.STM (simHasFS)
import           Test.Util.Tracer

-- | Setup a network of core nodes, where each joins according to the node join
-- plan and is interconnected according to the node topology
--
-- We run for the specified number of blocks, then return the final state of
-- each node.
runNodeNetwork :: forall m blk.
                    ( IOLike m
                    , RunNode blk
                    , TxGen blk
                    , TracingConstraints blk
                    , HasCallStack
                    )
                 => ResourceRegistry m
                 -> TestBlockchainTime m
                 -> NumCoreNodes
                 -> NodeJoinPlan
                 -> NodeTopology
                 -> (CoreNodeId -> ProtocolInfo blk)
                 -> ChaChaDRG
                 -> DiffTime
                 -> m (TestOutput blk)
runNodeNetwork registry0 testBtime numCoreNodes nodeJoinPlan nodeTopology
  pInfo initRNG slotLen = do
    -- This function is organized around the notion of a network of nodes as a
    -- simple graph with no loops. The graph topology is determined by
    -- @nodeTopology@.
    --
    -- Each graph node is a Ouroboros core node, with its own private threads
    -- managing the node's internal state. Some nodes join the network later
    -- than others, according to @nodeJoinPlan@.
    --
    -- Each undirected edge denotes two opposing directed edges. Each directed
    -- edge denotes a bundle of mini protocols with client threads on the tail
    -- node and server threads on the head node. These mini protocols begin as
    -- soon as both nodes have joined the network, according to @nodeJoinPlan@.

    varRNG <- uncheckedNewTVarM initRNG

    -- allocate a variable to collect the node instances for each CoreNodeId
    nodeVars <- fmap Map.fromList $ do
      forM coreNodeIds $ \nid -> (,) nid <$>
        uncheckedNewTVarM NodeInstances
          { nisAccessors = Map.empty
          , nisDown      = Nothing
          }
    let _ = nodeVars :: Map CoreNodeId (StrictTVar m (NodeInstances m blk))

    -- spawn threads for each undirected edge
    let edges = edgesNodeTopology nodeTopology
    forM_ edges $ \edge -> do
      void $ forkLinkedThread registry0 $ do
        undirectedEdge nullDebugTracer nodeVars edge

    -- create nodes
    let nodesByJoinSlot =
          List.sortOn fst $   -- sort non-descending by join slot
          map (\nv@(n, _) -> (joinSlotOf n, nv)) $
          Map.toList nodeVars
    nodes <- forM nodesByJoinSlot $ \(joinSlot, (coreNodeId, nodeVar)) -> do
      -- do not start the node before its joinSlot
      tooLate <- blockUntilSlot btime joinSlot
      when tooLate $ do
        error $ "unsatisfiable nodeJoinPlan: " ++ show coreNodeId

      -- spawn a thread representing the vertex in the topology
      void $ forkLinkedThread registry0 $ do
        -- allocate state variables specific to this vertex in the topology, to
        -- be reused by every node instance of it
        (nodeInfo, readNodeInfo) <- newNodeInfo

        -- spawn a thread representing the node instance, and respawn one each
        -- time it fails due to 'StopNodeInstance'
        --
        -- We must use a separate thread, because it must terminate
        -- exceptionally and via @link@ notify the corresponding mini protocol
        -- threads
        --
        -- NOTE We do *NOT* link this thread to 'registry0', because we expect
        -- it to fail.
        let tag = "NODE " <> show coreNodeId
        loopOnStopNodeInstance tag $
          -- if the node instance is already down, replace relevant exceptions
          -- by StopNodeInstance: one of other clean-up routines in the node's
          -- registry has failed due to the rest of the node having already
          -- closed
          switchToStopNodeInstance tag (NodeStopper coreNodeId nodeVar) $ do
            void $ (>>= waitThread) $ forkThread registry0 $ withRegistry $ \registry1 -> do
              s <- atomically $ getCurrentSlot btime
--              traceWith debugTracer $ "New NodeInstance " <> show (coreNodeId, s)

              -- spawn the node's internal threads
              (node, app, monitorLoop) <-
                createNode varRNG coreNodeId nodeInfo registry1

              -- TODO: use the RekeyPlan here instead of these hardwired restarts
              proxy <- forkLinkedThread registry1 $ do
                let s' = s + SlotNo 2
                _ <- blockUntilSlot btime s'

--                traceWith debugTracer $ "XXX " <> show (StopNodeInstance coreNodeId s')

                atomically $ modifyTVar nodeVar $ downNodeInstances s'
                ChainDB.closeDB $ getChainDB node

                throwM $ StopNodeInstance coreNodeId s'

              -- "export" access to this node for use by the mini protocol threads
              -- and the test framework's inspection
              atomically $ modifyTVar nodeVar $ insertNodeInstances s $
                NodeInstance
                  { niApp      = app
                  , niKernel   = node
                  , niProxy    = proxy
                  , niReadInfo = readNodeInfo
                  }

              monitorLoop

      return (coreNodeId, pInfoConfig (pInfo coreNodeId), nodeVar)

    waitForTest

    -- Close the 'ResourceRegistry': this shuts down the background threads of
    -- a node. This is important because we close the ChainDBs in
    -- 'getTestOutput' and if background threads that use the ChainDB are
    -- still running at that point, they will throw a 'CloseDBError'.
    closeRegistry registry0

    getTestOutput nodes
  where
    btime = testBlockchainTime testBtime

    waitForTest = do
      -- Wait some extra time after the end of the test block fetch and chain
      -- sync to finish
      testBlockchainTimeDone testBtime
      threadDelay 2000   -- arbitrary "small" duration

    coreNodeIds :: [CoreNodeId]
    coreNodeIds = enumCoreNodes numCoreNodes

    joinSlotOf :: CoreNodeId -> SlotNo
    joinSlotOf = coreNodeIdJoinSlot nodeJoinPlan

    undirectedEdge ::
         HasCallStack
      => Tracer m (SlotNo, MiniProtocolState, MiniProtocolExpectedException blk)
      -> Map CoreNodeId (StrictTVar m (NodeInstances m blk))
      -> (CoreNodeId, CoreNodeId)
      -> m ()
    undirectedEdge tr nodeVars (node1, node2) = loopOnStopNodeInstance ("EDGE " <> show (node1, node2)) $ do
      let (nv1, nv2) = (get node1, get node2)
            where
              get node = case Map.lookup node nodeVars of
                Nothing -> error $ "node not found: " ++ show node
                Just nv -> nv

      -- block until both node instances are up
      (ni1, ni2) <- atomically $ do
        let get nv = do
              NodeInstances{nisAccessors, nisDown} <- readTVar nv
              case Map.lookupMax nisAccessors of
                Just (_, ni : _) | Nothing <- nisDown -> pure ni
                _                                     -> retry
        (,) <$> get nv1 <*> get nv2

      -- tear down the undirected edge if either node instance fails
      --
      -- The only non-fatal exception conveyed by these links is
      -- StopNodeInstance.
      linkThread $ niProxy ni1
      linkThread $ niProxy ni2

      let (de12, de21) = (get v1 v2, get v2 v1)
            where
              v1 = (node1, ni1)
              v2 = (node2, ni2)
              get me them =
                -- replace reasonable exceptions from this vertex of the edge
                -- by StopNodeInstance if its node instance is already down:
                -- it's just failing because the node is suddenly absent
                switchToStopNodeInstance
                  (show (fst me, fst them))
                  (EdgeStopper node1 nv1 node2 nv2) $
                directedEdge tr btime me them

      -- NOTE 'directedEdge' only terminates by exception
      withAsyncsWaitAny $ de12 NE.:| [de21]

    -- | Produce transactions every time the slot changes and submit them to
    -- the mempool.
    txProducer :: HasCallStack
               => NodeConfig (BlockProtocol blk)
               -> ResourceRegistry m
               -> m ChaChaDRG
                  -- ^ How to get a DRG
               -> STM m (ExtLedgerState blk)
                  -- ^ How to get the current ledger state
               -> Mempool m blk TicketNo
               -> m ()
    txProducer cfg registry1 produceDRG getExtLedger mempool =
      onSlotChange registry1 btime $ \_curSlotNo -> do
        varDRG <- uncheckedNewTVarM =<< produceDRG
        txs <- atomically $ do
          ledger <- ledgerState <$> getExtLedger
          simChaChaT varDRG id $ testGenTxs numCoreNodes cfg ledger
        void $ addTxs mempool txs

    mkArgs :: NodeConfig (BlockProtocol blk)
           -> ExtLedgerState blk
           -> EpochInfo m
           -> Tracer m (Point blk)
              -- ^ invalid block tracer
           -> Tracer m (Point blk, BlockNo)
              -- ^ added block tracer
           -> NodeDBs (StrictTVar m MockFS)
           -> ResourceRegistry m
           -> ChainDbArgs m blk
    mkArgs
      cfg initLedger epochInfo
      invalidTracer addTracer
      nodeDBs registry1 = ChainDbArgs
        { -- Decoders
          cdbDecodeHash       = nodeDecodeHeaderHash (Proxy @blk)
        , cdbDecodeBlock      = nodeDecodeBlock cfg
        , cdbDecodeLedger     = nodeDecodeLedgerState cfg
        , cdbDecodeChainState = nodeDecodeChainState (Proxy @blk)
          -- Encoders
        , cdbEncodeBlock      = nodeEncodeBlock cfg
        , cdbEncodeHash       = nodeEncodeHeaderHash (Proxy @blk)
        , cdbEncodeLedger     = nodeEncodeLedgerState cfg
        , cdbEncodeChainState = nodeEncodeChainState (Proxy @blk)
          -- Error handling
        , cdbErrImmDb         = EH.monadCatch
        , cdbErrVolDb         = EH.monadCatch
        , cdbErrVolDbSTM      = EH.throwSTM
          -- HasFS instances
        , cdbHasFSImmDb       = simHasFS EH.monadCatch (nodeDBsImm nodeDBs)
        , cdbHasFSVolDb       = simHasFS EH.monadCatch (nodeDBsVol nodeDBs)
        , cdbHasFSLgrDB       = simHasFS EH.monadCatch (nodeDBsLgr nodeDBs)
          -- Policy
        , cdbValidation       = ImmDB.ValidateAllEpochs
        , cdbBlocksPerFile    = 4
        , cdbParamsLgrDB      = LgrDB.ledgerDbDefaultParams (protocolSecurityParam cfg)
        , cdbDiskPolicy       = LgrDB.defaultDiskPolicy (protocolSecurityParam cfg) slotLen
          -- Integration
        , cdbNodeConfig       = cfg
        , cdbEpochInfo        = epochInfo
        , cdbIsEBB            = \blk -> if nodeIsEBB blk
                                        then Just (blockHash blk)
                                        else Nothing
        , cdbGenesis          = return initLedger
        -- Misc
        , cdbTracer           = Tracer $ \case
              ChainDB.TraceAddBlockEvent
                  (ChainDB.AddBlockValidation ChainDB.InvalidBlock
                      { _invalidPoint = p })
                  -> traceWith invalidTracer p
              ChainDB.TraceAddBlockEvent
                  (ChainDB.AddedBlockToVolDB p bno IsNotEBB)
                  -> traceWith addTracer (p, bno)
              _   -> pure ()
        , cdbTraceLedger      = nullTracer
        , cdbRegistry         = registry1
        , cdbGcDelay          = 0
        }

    createNode
      :: HasCallStack
      => StrictTVar m ChaChaDRG
      -> CoreNodeId
      -> NodeInfo blk (StrictTVar m MockFS) (Tracer m)
      -> ResourceRegistry m
      -> m ( NodeKernel m NodeId blk
           , LimitedApp m NodeId blk
           , m ()
           )
    createNode varRNG coreNodeId nodeInfo registry1 = do
      let ProtocolInfo{..} = pInfo coreNodeId

      let callbacks :: NodeCallbacks m blk
          callbacks = NodeCallbacks {
              produceBlock = \proof _l slot prevPoint prevNo txs -> do
                let curNo :: BlockNo
                    curNo = succ prevNo

                let prevHash :: ChainHash blk
                    prevHash = castHash (pointHash prevPoint)

                nodeForgeBlock pInfoConfig
                               slot
                               curNo
                               prevHash
                               txs
                               proof

            , produceDRG      = atomically $ simChaChaT varRNG id $ drgNew
            }

      let NodeInfo
            { nodeInfoEvents
            , nodeInfoDBs
            } = nodeInfo

      epochInfo <- newEpochInfo $ nodeEpochSize (Proxy @blk) pInfoConfig
      chainDB <- ChainDB.openDB $ mkArgs
          pInfoConfig pInfoInitLedger epochInfo
          (nodeEventsInvalids nodeInfoEvents)
          (Tracer $ \(p, bno) -> do
              s <- atomically $ getCurrentSlot btime
              traceWith (nodeEventsAdds nodeInfoEvents) (s, p, bno))
          nodeInfoDBs
          registry1

      let nodeArgs = NodeArgs
            { tracers             = nullDebugTracers
                { forgeTracer = nodeEventsForges nodeInfoEvents
                }
            , registry            = registry1
            , maxClockSkew        = ClockSkew 1
            , cfg                 = pInfoConfig
            , initState           = pInfoInitState
            , btime
            , chainDB
            , callbacks
            , blockFetchSize      = nodeBlockFetchSize
            , blockMatchesHeader  = nodeBlockMatchesHeader
            , maxUnackTxs         = 1000 -- TODO
            , mempoolCap          = MempoolCapacity 10 -- TODO
            , chainSyncPipelining = pipelineDecisionLowHighMark 2 4
            }

      nodeKernel <- initNodeKernel nodeArgs
      let app = consensusNetworkApps
                  nodeKernel
                  nullDebugProtocolTracers{ptTxSubmissionTracer = nullTracer}
                  protocolCodecsId
                  (protocolHandlers nodeArgs nodeKernel)

      let monitorLoop s = do
            -- TODO We assume this effectively runs before anything else in the
            -- slot. With such a short transaction (read one TVar) this is likely
            -- but not necessarily certain.
            s' <- atomically $ do
              s' <- getCurrentSlot btime
              check (s /= s')
              pure s'

            bno <- atomically $ ChainDB.getTipBlockNo chainDB
            traceWith (nodeEventsTipBlockNos nodeInfoEvents) (s', bno)

            monitorLoop s'

      void $ forkLinkedThread registry1 $ txProducer
        pInfoConfig
        registry1
        (produceDRG callbacks)
        (ChainDB.getCurrentLedger chainDB)
        (getMempool nodeKernel)

      s0 <- atomically $ getCurrentSlot btime
      return (nodeKernel, LimitedApp app, monitorLoop s0)

data NodeInstance m blk = NodeInstance
  { niApp       :: !(LimitedApp m NodeId blk)
  , niKernel    :: !(NodeKernel m NodeId blk)
  , niProxy     :: !(Thread m ())
  , niReadInfo  :: !(m (NodeInfo blk MockFS []))
  }

data NodeInstances m blk = NodeInstances
  { nisAccessors :: !(Map SlotNo [NodeInstance m blk])
  , nisDown      :: !(Maybe SlotNo)
  }

insertNodeInstances ::
     SlotNo -> NodeInstance m blk -> NodeInstances m blk -> NodeInstances m blk
insertNodeInstances s ni nis = nis
  { nisAccessors =
      Map.alter (Just . (ni :) . fromMaybe []) s (nisAccessors nis)
  , nisDown      = Nothing
  }

downNodeInstances :: SlotNo -> NodeInstances m blk -> NodeInstances m blk
downNodeInstances s nis = nis{nisDown = Just s}

getLatestNodeInstances :: NodeInstances m blk -> Maybe (SlotNo, NodeInstance m blk)
getLatestNodeInstances NodeInstances{nisAccessors} =
    case Map.lookupMax nisAccessors of
      Just (s, ni : _) -> Just (s, ni)
      _                -> Nothing

data StopNodeInstance = StopNodeInstance CoreNodeId SlotNo
  deriving (Show)

instance Exception StopNodeInstance

data ExnCont a
  = ExnContAgain
  | ExnContDone a
  | ExnContRethrow SomeException
  deriving (Show)

loopOnStopNodeInstance ::
  forall m a. (IOLike m, Show a) => String -> m a -> m a
loopOnStopNodeInstance s m = do
    x <- catch (ExnContDone <$> m) handler
--    traceWith debugTracer (s <> " " <> show x)
    case x of
      ExnContAgain     -> loopOnStopNodeInstance s m
      ExnContDone a    -> pure a
      ExnContRethrow e -> throwM e
  where
    handler :: SomeException -> m (ExnCont a)
    handler e = pure $ fromMaybe (ExnContRethrow e) (h e)

    h :: SomeException -> Maybe (ExnCont a)
    h e
      | Just e2 <- fromLinkedExn e = h e2
      | Just e2 <- fromException e = Just $ hStopping e2
      | otherwise                  = Nothing

    hStopping StopNodeInstance{} = ExnContAgain

data Stoppers m blk
  = NodeStopper CoreNodeId (StrictTVar m (NodeInstances m blk))
  | EdgeStopper CoreNodeId (StrictTVar m (NodeInstances m blk))
                CoreNodeId (StrictTVar m (NodeInstances m blk))

switchToStopNodeInstance ::
  forall blk m a.
     ( IOLike m
     , SupportedBlock blk
     )
  => String
  -> Stoppers m blk
  -> m a
  -> m a
switchToStopNodeInstance _s stoppers m =
    catch m $ \e -> do
      e' <- fromMaybe e <$> switch Nothing e
--      traceWith debugTracer (s <> " " <> show e')
      throwM e'
  where
    replace' coreNodeId nodeVar = do
      NodeInstances{nisDown} <- atomically $ readTVar nodeVar
      pure $ (toException . StopNodeInstance coreNodeId ) <$> nisDown

    replace me = case stoppers of
      NodeStopper coreNodeId nodeVar -> replace' coreNodeId nodeVar
      EdgeStopper n1 nv1 n2 nv2
        | Just n1 == me -> replace' n1 nv1
        | Just n2 == me -> replace' n2 nv2
        | otherwise     -> pure Nothing

    switch me e
      | Just e2                <- fromLinkedExn e = switch me e2
      | Just (DirectedEdgeException _ n e2) <- e' = switch (Just n) e2

      | Just StopNodeInstance{}             <- e' = pure $ Just e

      -- we close the DB when stopping the node, so this is definitely
      -- plausible
      | Just ChainDB.ClosedDBError{}      <- cdbE = replace me

      | otherwise                                 = pure Nothing
      where
        e' :: forall exn. Exception exn => Maybe exn
        e' = fromException e

        cdbE :: Maybe (ChainDB.ChainDbError blk)
        cdbE = e'

{-------------------------------------------------------------------------------
  Running the Mini Protocols on an Ordered Pair of Nodes
-------------------------------------------------------------------------------}

-- | Spawn all mini protocols' threads for a given directed edge in the node
-- network topology (ie an ordered pair of core nodes, client first, server
-- second)
--
-- Key property: if any client thread or server thread in any of the mini
-- protocols throws an exception, restart all of the threads.
--
-- The actual node implementation kills the other threads on the same peer as
-- the thread that threw the exception, and then relies on TCP socket semantics
-- to eventually kill the corresponding threads on the remote peer. The client
-- node recreates its client threads after a delay, and they reconnect to the
-- remote peer, thereby recreating the server threads.
--
-- This mock network instead ensures the property directly via the async
-- interface rather than relying on some sort of mock socket semantics to
-- convey the cancellation.
--
-- It only catches-and-restarts on /expected/ exceptions; anything else will
-- tear down the whole hierarchy of test threads. See
-- 'MiniProtocolExpectedException'.
directedEdge ::
  forall m blk. (IOLike m, SupportedBlock blk)
  => Tracer m (SlotNo, MiniProtocolState, MiniProtocolExpectedException blk)
  -> BlockchainTime m
  -> (CoreNodeId, NodeInstance m blk)
  -> (CoreNodeId, NodeInstance m blk)
  -> m ()
directedEdge tr btime nodeapp1 nodeapp2 =
    loopOnMPEE
  where
    loopOnMPEE = do
         ec <- (ExnContDone <$> directedEdgeInner nodeapp1 nodeapp2) `catch` handler
         case ec of
           ExnContAgain     -> loopOnMPEE
           ExnContDone a    -> pure a
           ExnContRethrow e -> throwM e
      where
        handler :: SomeException -> m (ExnCont ())
        handler e = fromMaybe (pure (ExnContRethrow e)) (h e)

        h :: SomeException -> Maybe (m (ExnCont ()))
        h e
          | Just (DirectedEdgeException _ _ e2) <- e' = h e2
          | Just e2                             <- e' = Just $ hExpected e2
          | otherwise                                 = Nothing
          where
            e' :: forall exn. Exception exn => Maybe exn
            e' = fromException e

        -- Catch and restart on expected exceptions
        --
        hExpected :: MiniProtocolExpectedException blk -> m (ExnCont ())
        hExpected e = do
          s@(SlotNo i) <- atomically $ getCurrentSlot btime
          traceWith tr (s, MiniProtocolDelayed, e)
          void $ blockUntilSlot btime $ SlotNo (succ i)
          traceWith tr (s, MiniProtocolRestarting, e)
          pure ExnContAgain

-- | Spawn threads for all of the mini protocols
--
-- See 'directedEdge'.
directedEdgeInner ::
  forall m blk. (IOLike m, SupportedBlock blk)
  => (CoreNodeId, NodeInstance m blk)
     -- ^ client threads on this node
  -> (CoreNodeId, NodeInstance m blk)
     -- ^ server threads on this node
  -> m ()
directedEdgeInner
  (node1, NodeInstance{niApp = LimitedApp app1})
  (node2, NodeInstance{niApp = LimitedApp app2}) = do
    void $ (>>= withAsyncsWaitAny) $
      fmap flattenPairs $
      sequence $
      ( miniProtocol
          (wrapMPEE MPEEChainSyncClient naChainSyncClient)
          naChainSyncServer
      ) NE.:|
      [ miniProtocol
          (wrapMPEE MPEEBlockFetchClient naBlockFetchClient)
          (wrapMPEE MPEEBlockFetchServer naBlockFetchServer)
      , miniProtocol
          (wrapMPEE MPEETxSubmissionClient naTxSubmissionClient)
          (wrapMPEE MPEETxSubmissionServer naTxSubmissionServer)
      ]
  where
    flattenPairs :: forall a. NE.NonEmpty (a, a) -> NE.NonEmpty a
    flattenPairs = uncurry (<>) . NE.unzip

    miniProtocol ::
         (forall unused1 unused2.
            LimitedApp' m NodeId blk unused1 unused2
         -> NodeId
         -> Channel m msg
         -> m ())
        -- ^ client action to run on node1
      -> (forall unused1 unused2.
            LimitedApp' m NodeId blk unused1 unused2
         -> NodeId
         -> Channel m msg
         -> m ())
         -- ^ server action to run on node2
      -> m (m (), m ())
    miniProtocol client server = do
       (chan, dualChan) <- createConnectedChannels
       pure
         ( wrapDE node1 $ client app1 (fromCoreNodeId node2) chan
         , wrapDE node2 $ server app2 (fromCoreNodeId node1) dualChan
         )

    wrapDE :: CoreNodeId -> m a -> m a
    wrapDE n m = catch m $ \e -> throwM $ h e e
      where
        h e
          | Just e2 <- fromLinkedExn e = h e2
          | isAsyncExn e               = id
          | otherwise                  =
            toException . DirectedEdgeException (node1, node2) n

    wrapMPEE ::
         Exception e
      => (e -> MiniProtocolExpectedException blk)
      -> (app -> peer -> chan -> m a)
      -> (app -> peer -> chan -> m a)
    wrapMPEE f m = \app them chan ->
        catch (m app them chan) $ throwM . f

{-------------------------------------------------------------------------------
  Node Info
-------------------------------------------------------------------------------}

data NodeInfo blk db ev = NodeInfo
  { nodeInfoEvents :: NodeEvents blk ev
  , nodeInfoDBs    :: NodeDBs db
  }

-- | A vector with an @ev@-shaped element for a particular set of
-- instrumentation events
--
-- The @ev@ type parameter is instantiated by this module at types for
-- 'Tracer's and lists: actions for accumulating and lists as accumulations.
data NodeEvents blk ev = NodeEvents
  { nodeEventsAdds        :: ev (SlotNo, Point blk, BlockNo)
    -- ^ every 'AddedBlockToVolDB' excluding EBBs
  , nodeEventsForges      :: ev (TraceForgeEvent blk)
    -- ^ every 'TraceForgeEvent'
  , nodeEventsInvalids    :: ev (Point blk)
    -- ^ the point of every 'ChainDB.InvalidBlock' event
  , nodeEventsTipBlockNos :: ev (SlotNo, BlockNo)
    -- ^ 'ChainDB.getTipBlockNo' for each node at the onset of each slot
  }

-- | A vector with an element for each database of a node
--
-- The @db@ type parameter is instantiated by this module at types for mock
-- filesystems; either the 'MockFS' type or reference cells thereof.
data NodeDBs db = NodeDBs
  { nodeDBsImm :: db
  , nodeDBsVol :: db
  , nodeDBsLgr :: db
  }

newNodeInfo ::
  forall blk m.
     IOLike m
  => m ( NodeInfo blk (StrictTVar m MockFS) (Tracer m)
       , m (NodeInfo blk MockFS [])
       )
newNodeInfo = do
  (nodeInfoEvents, readEvents) <- do
      (t1, m1) <- recordingTracerTVar
      (t2, m2) <- recordingTracerTVar
      (t3, m3) <- recordingTracerTVar
      (t4, m4) <- recordingTracerTVar
      pure
          ( NodeEvents     t1     t2     t3     t4
          , NodeEvents <$> m1 <*> m2 <*> m3 <*> m4
          )

  (nodeInfoDBs, readDBs) <- do
      let mk :: m (StrictTVar m MockFS, STM m MockFS)
          mk = do
              v <- uncheckedNewTVarM Mock.empty
              pure (v, readTVar v)
      (v1, m1) <- mk
      (v2, m2) <- mk
      (v3, m3) <- mk
      pure
          ( NodeDBs     v1     v2     v3
          , NodeDBs <$> m1 <*> m2 <*> m3
          )

  pure
      ( NodeInfo{nodeInfoEvents, nodeInfoDBs}
      , NodeInfo <$> readEvents <*> atomically readDBs
      )

{-------------------------------------------------------------------------------
  Test Output - output data about each node
-------------------------------------------------------------------------------}

data NodeOutput blk = NodeOutput
  { nodeOutputAdds       :: Map SlotNo (Set (Point blk, BlockNo))
  , nodeOutputCfg        :: NodeConfig (BlockProtocol blk)
  , nodeOutputFinalChain :: Chain blk
  , nodeOutputNodeDBs    :: NodeDBs MockFS
  , nodeOutputForges     :: Map SlotNo blk
  , nodeOutputInvalids   :: Set (Point blk)
  }

data TestOutput blk = TestOutput
    { testOutputNodes       :: Map NodeId (NodeOutput blk)
    , testOutputTipBlockNos :: Map SlotNo (Map NodeId BlockNo)
    }

-- | Gather the test output from the nodes
getTestOutput ::
    forall m blk.
       ( IOLike m
       , HasHeader blk
       , HasCallStack
       )
    => [( CoreNodeId
        , NodeConfig (BlockProtocol blk)
        , StrictTVar m (NodeInstances m blk)
        )]
    -> m (TestOutput blk)
getTestOutput nodes = do
    (nodeOutputs', tipBlockNos') <- fmap unzip $ forM nodes $
      \(cid, cfg, nodeVar) -> do
        nis <- atomically $ readTVar nodeVar
        (node, readNodeInfo) <- case getLatestNodeInstances nis of
          Just (_, NodeInstance{niKernel, niReadInfo}) ->
            pure (niKernel, niReadInfo)
          _ -> error "TODO"

        let nid = fromCoreNodeId cid
        let chainDB = getChainDB node
        ch <- ChainDB.toChain chainDB
        ChainDB.closeDB chainDB
        nodeInfo <- readNodeInfo
        let NodeInfo
              { nodeInfoEvents
              , nodeInfoDBs
              } = nodeInfo
        let NodeEvents
              { nodeEventsAdds
              , nodeEventsForges
              , nodeEventsInvalids
              , nodeEventsTipBlockNos
              } = nodeInfoEvents
        let nodeOutput = NodeOutput
              { nodeOutputAdds       =
                  Map.fromListWith Set.union $
                  [ (s, Set.singleton (p, bno)) | (s, p, bno) <- nodeEventsAdds ]
              , nodeOutputCfg        = cfg
              , nodeOutputFinalChain = ch
              , nodeOutputNodeDBs    = nodeInfoDBs
              , nodeOutputForges     =
                  Map.fromList $
                  [ (s, b) | TraceForgeEvent s b <- nodeEventsForges ]
              , nodeOutputInvalids   = Set.fromList nodeEventsInvalids
              }

        pure
          ( Map.singleton nid nodeOutput
          , Map.singleton nid <$> Map.fromList nodeEventsTipBlockNos
          )

    pure $ TestOutput
        { testOutputNodes       = Map.unions nodeOutputs'
        , testOutputTipBlockNos = Map.unionsWith Map.union tipBlockNos'
        }

{-------------------------------------------------------------------------------
  Constraints needed for verbose tracing
-------------------------------------------------------------------------------}

nullDebugTracer :: (Applicative m, Show a) => Tracer m a
nullDebugTracer = nullTracer `asTypeOf` showTracing debugTracer

nullDebugTracers ::
     ( Monad m
     , Show peer
     , SupportedBlock blk
     , TracingConstraints blk
     )
  => Tracers m peer blk
nullDebugTracers = nullTracers `asTypeOf` showTracers debugTracer

nullDebugProtocolTracers ::
     ( Monad m
     , HasHeader blk
     , TracingConstraints blk
     , Show peer
     , Show failure
     )
  => ProtocolTracers m peer blk failure
nullDebugProtocolTracers =
  asTypeOf nullProtocolTracers $ showProtocolTracers debugTracer

-- These constraints are when using @showTracer(s) debugTracer@ instead of
-- @nullTracer(s)@.
type TracingConstraints blk =
  ( Show blk
  , Show (ApplyTxErr blk)
  , Show (Header blk)
  , Show (GenTx blk)
  , Show (GenTxId blk)
  )

{-------------------------------------------------------------------------------
  Ancillaries
-------------------------------------------------------------------------------}

-- | Spawn multiple async actions and wait for the first one to terminate
--
-- Each child thread is spawned with 'withAsync' and so won't outlive this one.
-- In the use case where each child thread only terminates on an exception, the
-- 'waitAny' ensures that this parent thread will also only terminate on
-- exception; re-raising either one that terminated a child thread or some
-- other \"external\" exception.
--
-- Why 'NE.NonEmpty'? An empty argument list would have blocked indefinitely,
-- which is likely not intended.
withAsyncsWaitAny :: forall m a. IOLike m => NE.NonEmpty (m a) -> m a
withAsyncsWaitAny = go [] . NE.toList
  where
    go acc = \case
      []   -> snd <$> waitAny acc
      m:ms -> withAsync m $ \h -> go (h:acc) ms

-- | The partially instantiation of the 'NetworkApplication' type according to
-- its use in this module
--
-- Used internal to this module, essentially as an abbreviatiation.
data LimitedApp m peer blk =
   forall unused1 unused2.
   LimitedApp (LimitedApp' m peer blk unused1 unused2)

-- | Argument of 'LimitedApp' data constructor
--
-- Used internal to this module, essentially as an abbreviatiation.
type LimitedApp' m peer blk unused1 unused2 =
    NetworkApplication m peer
        (AnyMessage (ChainSync (Header blk) (Tip blk)))
        (AnyMessage (BlockFetch blk))
        (AnyMessage (TxSubmission (GenTxId blk) (GenTx blk)))
        unused1 -- the local node-to-client channel types
        unused2
        ()

{-------------------------------------------------------------------------------
  Tracing
-------------------------------------------------------------------------------}

-- | Non-fatal exceptions expected from the threads of a 'directedEdge'
--
data MiniProtocolExpectedException blk
  = MPEEChainSyncClient (CSClient.ChainSyncClientException blk (Tip blk))
    -- ^ see "Ouroboros.Consensus.ChainSyncClient"
    --
    -- NOTE: the second type in 'ChainSyncClientException' denotes the 'tip'.
    -- If it does not agree with the consensus client & server, 'Dynamic chain
    -- generation' tests will fail, since they will not catch the right
    -- exception.
  | MPEEBlockFetchClient BFClient.BlockFetchProtocolFailure
    -- ^ see "Ouroboros.Network.BlockFetch.Client"
  | MPEEBlockFetchServer (BFServer.BlockFetchServerException blk)
    -- ^ see "Ouroboros.Consensus.BlockFetchServer"
  | MPEETxSubmissionClient TxOutbound.TxSubmissionProtocolError
    -- ^ see "Ouroboros.Network.TxSubmission.Outbound"
  | MPEETxSubmissionServer TxInbound.TxSubmissionProtocolError
    -- ^ see "Ouroboros.Network.TxSubmission.Inbound"
  deriving (Show)

instance (SupportedBlock blk) => Exception (MiniProtocolExpectedException blk)

data MiniProtocolState = MiniProtocolDelayed | MiniProtocolRestarting
  deriving (Show)

data TraceMiniProtocolRestart peer blk
  = TraceMiniProtocolRestart
      peer peer
      SlotNo
      MiniProtocolState
      (MiniProtocolExpectedException blk)
    -- ^ us them when-start-blocking state reason
  deriving (Show)

-- | A wrapper for almost all exceptions raised by a mini protocol thread
--
-- The only exceptions not wrapped are those that cast to
-- 'Exn.SomeAsyncException' even after peeling away layers of
-- 'Async.ExceptionInLinkedThread'.
--
data DirectedEdgeException = DirectedEdgeException
    !(CoreNodeId, CoreNodeId) !CoreNodeId !SomeException
  -- ^ the edge, the node that threw, the exception
  deriving (Show)

instance Exception DirectedEdgeException

fromLinkedExn :: SomeException -> Maybe SomeException
fromLinkedExn e =
    (\(Async.ExceptionInLinkedThread _ e2) -> e2) <$> fromException e

isAsyncExn :: SomeException -> Bool
isAsyncExn = isJust . fromAsyncExn

fromAsyncExn :: SomeException -> Maybe Exn.SomeAsyncException
fromAsyncExn = fromException
