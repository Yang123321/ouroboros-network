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
    broadcastNetwork
  , TracingConstraints
    -- * Test Output
  , TestOutput (..)
  , NodeOutput (..)
  , NodeInfo (..)
  ) where

import           Control.Monad
import           Control.Tracer
import           Crypto.Random (ChaChaDRG, drgNew)
import           Data.Foldable (traverse_)
import qualified Data.List as List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Proxy (Proxy (..))
import           GHC.Stack

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork (MonadFork)
import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer

import           Network.TypedProtocol.Channel
import           Network.TypedProtocol.Codec (AnyMessage (..))

import           Ouroboros.Network.Block
import           Ouroboros.Network.MockChain.Chain

import           Ouroboros.Network.Protocol.BlockFetch.Type
import           Ouroboros.Network.Protocol.ChainSync.Type
import           Ouroboros.Network.Protocol.TxSubmission.Type

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.ChainSyncClient (ClockSkew (..))
import           Ouroboros.Consensus.Ledger.Abstract
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
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Consensus.Util.RedundantConstraints
import           Ouroboros.Consensus.Util.ResourceRegistry
import           Ouroboros.Consensus.Util.STM

import qualified Ouroboros.Storage.ChainDB as ChainDB
import           Ouroboros.Storage.ChainDB.Impl (ChainDbArgs (..))
import           Ouroboros.Storage.EpochInfo (EpochInfo, newEpochInfo)
import           Ouroboros.Storage.FS.Sim.MockFS (MockFS)
import qualified Ouroboros.Storage.FS.Sim.MockFS as Mock
import           Ouroboros.Storage.FS.Sim.STM (simHasFS)
import qualified Ouroboros.Storage.ImmutableDB as ImmDB
import qualified Ouroboros.Storage.LedgerDB.DiskPolicy as LgrDB
import qualified Ouroboros.Storage.LedgerDB.MemPolicy as LgrDB
import qualified Ouroboros.Storage.Util.ErrorHandling as EH

import           Test.Dynamic.TxGen
import           Test.Dynamic.Util.NodeJoinPlan

-- | Interface provided by 'ouroboros-network'.  At the moment
-- 'ouroboros-network' only provides this interface in 'IO' backed by sockets,
-- we cook up here one using 'NodeChans'.
--
data NetworkInterface m peer = NetworkInterface {
      -- | Like 'Ouroboros.Network.NodeToNode.connectTo'
      --
      niConnectTo      :: peer -> m ()

      -- | Like 'Ouroboros.Network.NodeToNode.withServer'
      --
    , niWithServerNode :: forall t.  (Async m () -> m t) -> m t
    }

-- | Create 'NetworkInterface' from a map of channels between nodes.
--
-- TODO: move this function to 'ouroboros-network'.
--
createNetworkInterface
    :: forall m peer blk unused1 unused2.
       ( MonadAsync m
       , MonadMask  m
       , Ord peer
       )
    => BlockchainTime m
    -> NodeChans m peer blk -- ^ map of channels between nodes
    -> [peer]               -- ^ list of nodes which we want to serve
    -> (peer -> SlotNo)     -- ^ every node's join slot
    -> peer                 -- ^ our own peer identifier
    -> NetworkApplication m peer
        (AnyMessage (ChainSync (Header blk) (Point blk)))
        (AnyMessage (BlockFetch blk))
        (AnyMessage (TxSubmission (GenTxId blk) (GenTx blk)))
        unused1 -- the local node-to-client channel types
        unused2
        ()
    -> NetworkInterface m peer
createNetworkInterface btime chans nodeIds joinSlotOf us
                       NetworkApplication {
                         naChainSyncClient,
                         naChainSyncServer,
                         naBlockFetchClient,
                         naBlockFetchServer,
                         naTxSubmissionClient,
                         naTxSubmissionServer
                         -- Note that this test is not intended to cover the
                         -- mini-protocols in the node-to-client bundle, so
                         -- we don't pull those handlers out here.
                       } =
  NetworkInterface
    { niConnectTo = \them -> do
        let nodeChan = chans Map.! them Map.! us

        -- 'withAsync' guarantees that when 'waitAny' below receives an
        -- exception the threads will be killed.  If one of the threads will
        -- error, 'waitAny' will terminate and both threads will be killed (thus
        -- there's no need to use 'waitAnyCancel').
        withAsync (waitingFor them $ naChainSyncClient them
                        $ chainSyncProducer nodeChan)
                  $ \aCS ->
          withAsync (waitingFor them $ naBlockFetchClient them
                        $ blockFetchProducer nodeChan)
                  $ \aBF ->
            withAsync (waitingFor them $ naTxSubmissionClient them
                        $ txSubmissionProducer nodeChan)
                  $ \aTX ->
                    -- wait for all the threads, if any throws an exception, cancel all
                    -- of them; this is consistent with
                    -- 'Ouroboros.Network.Socket.connectTo'.
                    void $ waitAny [aCS, aBF, aTX]

      , niWithServerNode = \k -> mask $ \unmask -> do
        ts :: [Async m ()] <- fmap concat $ forM (filter (/= us) nodeIds) $ \them -> do
              let nodeChan = chans Map.! us Map.! them

              aCS <- async $ unmask
                           $ waitingFor them $ naChainSyncServer
                             them
                             (chainSyncConsumer nodeChan)
              aBF <- async $ unmask
                           $ waitingFor them $ naBlockFetchServer
                             them
                             (blockFetchConsumer nodeChan)
              aTX <- async $ unmask
                           $ waitingFor them $ naTxSubmissionServer
                             them
                             (txSubmissionConsumer nodeChan)

              return [aCS, aBF, aTX]

        -- if any thread raises an exception, kill all of them;
        -- if an exception is thrown to this thread, cancel all threads;
        (waitAnyCancel ts `onException` traverse_ cancel ts) >>= k . fst
    }
  where
    -- block until both us and our peer have joined the network
    waitingFor :: forall a. peer -> m a -> m ()
    waitingFor them m = void $ do
        tooLate <- blockUntilSlot btime $
            joinSlotOf us `maxSlot` joinSlotOf them
        when tooLate $
            error "createNetworkInterface: unsatisfiable nodeJoinPlan"
        void $ m

    maxSlot :: SlotNo -> SlotNo ->  SlotNo
    maxSlot (SlotNo i) (SlotNo j) = SlotNo (max i j)

-- | Setup fully-connected topology, where every node is both a producer
-- and a consumer
--
-- We run for the specified number of blocks, then return the final state of
-- each node.
broadcastNetwork :: forall m blk.
                    ( MonadAsync m
                    , MonadFork  m
                    , MonadMask  m
                    , MonadST    m
                    , MonadTime  m
                    , MonadTimer m
                    , MonadThrow (STM m)
                    , RunNode blk
                    , TxGen blk
                    , TracingConstraints blk
                    , HasCallStack
                    )
                 => ResourceRegistry m
                 -> TestBlockchainTime m
                 -> NumCoreNodes
                 -> NodeJoinPlan
                 -> (CoreNodeId -> ProtocolInfo blk)
                 -> ChaChaDRG
                 -> DiffTime
                 -> m (TestOutput blk)
broadcastNetwork registry testBtime numCoreNodes nodeJoinPlan pInfo initRNG slotLen = do
    chans :: NodeChans m NodeId blk <- createCommunicationChannels

    varRNG <- atomically $ newTVar initRNG

    nodes <- forM (List.sortOn joinSlotOf coreNodeIds) $ \coreNodeId -> do
      -- do not start the node before its joinSlot
      tooLate <- blockUntilSlot btime $ joinSlotOf coreNodeId
      when tooLate $ error "broadcastNetwork: unsatisfiable nodeJoinPlan"

      (node, nodeInfo) <- createAndConnectNode chans varRNG coreNodeId
      return (coreNodeId, pInfoConfig (pInfo coreNodeId), node, nodeInfo)

    -- Wait a random amount of time after the final slot for the block fetch
    -- and chain sync to finish
    testBlockchainTimeDone testBtime
    threadDelay 2000

    -- Close the 'ResourceRegistry': this shuts down the background threads of
    -- a node. This is important because we close the ChainDBs in
    -- 'getTestOutput' and if background threads that use the ChainDB are
    -- still running at that point, they will throw a 'CloseDBError'.
    closeRegistry registry

    getTestOutput nodes
  where
    _ = keepRedundantConstraint (Proxy @(TracingConstraints blk))

    btime = testBlockchainTime testBtime

    nodeIds :: [NodeId]
    nodeIds = map fromCoreNodeId coreNodeIds

    coreNodeIds :: [CoreNodeId]
    coreNodeIds = enumCoreNodes numCoreNodes

    joinSlotOf :: CoreNodeId -> SlotNo
    joinSlotOf nid = case nodeJoinPlan of
        NodeJoinPlan m -> Map.findWithDefault
            (error $ "broadcastNetwork: incomplete NodeJoinPlan: "
                 <> condense (nid, m))
            nid m

    -- | Produce transactions every time the slot changes and submit them to
    -- the mempool.
    txProducer :: HasCallStack
               => NodeConfig (BlockProtocol blk)
               -> m ChaChaDRG
                  -- ^ How to get a DRG
               -> STM m (ExtLedgerState blk)
                  -- ^ How to get the current ledger state
               -> Mempool m blk TicketNo
               -> m ()
    txProducer cfg produceDRG getExtLedger mempool =
      onSlotChange btime $ \_curSlotNo -> do
        drg <- produceDRG
        txs <- atomically $ do
          ledger <- ledgerState <$> getExtLedger
          varDRG <- newTVar drg
          simChaChaT varDRG id $ testGenTxs numCoreNodes cfg ledger
        void $ addTxs mempool txs

    createCommunicationChannels :: m (NodeChans m NodeId blk)
    createCommunicationChannels = fmap Map.fromList $ forM nodeIds $ \us ->
      fmap ((us, ) . Map.fromList) $ forM (filter (/= us) nodeIds) $ \them -> do
        (chainSyncConsumer,    chainSyncProducer)    <- createConnectedChannels
        (blockFetchConsumer,   blockFetchProducer)   <- createConnectedChannels
        (txSubmissionConsumer, txSubmissionProducer) <- createConnectedChannels
        return (them, NodeChan {..})

    mkArgs :: NodeConfig (BlockProtocol blk)
           -> ExtLedgerState blk
           -> EpochInfo m
           -> (StrictTVar m MockFS, StrictTVar m MockFS, StrictTVar m MockFS)
              -- ^ ImmutableDB, VolatileDB, LedgerDB
           -> ChainDbArgs m blk
    mkArgs cfg initLedger epochInfo (immDbFsVar, volDbFsVar, lgrDbFsVar) = ChainDbArgs
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
        , cdbHasFSImmDb       = simHasFS EH.monadCatch immDbFsVar
        , cdbHasFSVolDb       = simHasFS EH.monadCatch volDbFsVar
        , cdbHasFSLgrDB       = simHasFS EH.monadCatch lgrDbFsVar
          -- Policy
        , cdbValidation       = ImmDB.ValidateAllEpochs
        , cdbBlocksPerFile    = 4
        , cdbMemPolicy        = LgrDB.defaultMemPolicy  (protocolSecurityParam cfg)
        , cdbDiskPolicy       = LgrDB.defaultDiskPolicy (protocolSecurityParam cfg) slotLen
          -- Integration
        , cdbNodeConfig       = cfg
        , cdbEpochInfo        = epochInfo
        , cdbIsEBB            = \blk -> if nodeIsEBB blk
                                        then Just (blockHash blk)
                                        else Nothing
        , cdbGenesis          = return initLedger
        -- Misc
        , cdbTracer           = nullTracer
        , cdbRegistry         = registry
        , cdbGcDelay          = 0
        }

    createAndConnectNode
      :: HasCallStack
      => NodeChans m NodeId blk
      -> StrictTVar m ChaChaDRG
      -> CoreNodeId
      -> m (NodeKernel m NodeId blk, NodeInfo blk (StrictTVar m MockFS))
    createAndConnectNode chans varRNG coreNodeId = do
      let us               = fromCoreNodeId coreNodeId
          ProtocolInfo{..} = pInfo coreNodeId

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

      epochInfo <- newEpochInfo $ nodeEpochSize (Proxy @blk) pInfoConfig
      fsVars@(immDbFsVar, volDbFsVar, lgrDbFsVar)  <- atomically $ (,,)
        <$> newTVar Mock.empty <*> newTVar Mock.empty <*> newTVar Mock.empty
      let args = mkArgs pInfoConfig pInfoInitLedger epochInfo fsVars
      chainDB <- ChainDB.openDB args

      let nodeArgs = NodeArgs
            { tracers            = nullTracers
            , registry           = registry
            , maxClockSkew       = ClockSkew 1
            , cfg                = pInfoConfig
            , initState          = pInfoInitState
            , btime
            , chainDB
            , callbacks
            , blockFetchSize     = nodeBlockFetchSize
            , blockMatchesHeader = nodeBlockMatchesHeader
            , maxUnackTxs        = 1000 -- TODO
            }

      nodeKernel <- initNodeKernel nodeArgs
      let app = consensusNetworkApps
                  nodeKernel
                  nullProtocolTracers
                  protocolCodecsId
                  (protocolHandlers nodeArgs nodeKernel)

          ni :: NetworkInterface m NodeId
          ni = createNetworkInterface btime chans nodeIds jso us app
            where
              jso = \case
                CoreId  i -> joinSlotOf (CoreNodeId i)
                RelayId _ -> error "broadcastNetwork: unexpected RelayId"

      void $ forkLinkedThread registry $ niWithServerNode ni wait
      void $ forkLinkedThread registry $ txProducer
        pInfoConfig
        (produceDRG callbacks)
        (ChainDB.getCurrentLedger chainDB)
        (getMempool nodeKernel)

      forM_ (filter (/= us) nodeIds) $ \them ->
        void $ forkLinkedThread registry $ niConnectTo ni them

      let nodeInfo = NodeInfo
            { nodeInfoImmDbFs = immDbFsVar
            , nodeInfoVolDbFs = volDbFsVar
            , nodeInfoLgrDbFs = lgrDbFsVar
            }
      return (nodeKernel, nodeInfo)

{-------------------------------------------------------------------------------
  Node Info
-------------------------------------------------------------------------------}

data NodeInfo blk fs = NodeInfo
  { nodeInfoImmDbFs :: fs
  , nodeInfoVolDbFs :: fs
  , nodeInfoLgrDbFs :: fs
  }

readFsTVars :: MonadSTM m
            => NodeInfo blk (StrictTVar m MockFS)
            -> m (NodeInfo blk MockFS)
readFsTVars tvars = atomically $ NodeInfo
    <$> readTVar (nodeInfoImmDbFs tvars)
    <*> readTVar (nodeInfoVolDbFs tvars)
    <*> readTVar (nodeInfoLgrDbFs tvars)

{-------------------------------------------------------------------------------
  Test Output - records of how each node's chain changed
-------------------------------------------------------------------------------}

data NodeOutput blk = NodeOutput
  { nodeOutputCfg        :: NodeConfig (BlockProtocol blk)
  , nodeOutputFinalChain :: Chain blk
  , nodeOutputNodeInfo   :: NodeInfo blk MockFS
  }

newtype TestOutput blk = TestOutput
    { testOutputNodes :: Map NodeId (NodeOutput blk)
    }

-- | Gather the test output from the nodes
getTestOutput ::
    forall m blk.
       ( MonadSTM m
       , MonadMask m
       , MonadFork m
       , HasHeader blk
       )
    => [( CoreNodeId
        , NodeConfig (BlockProtocol blk)
        , NodeKernel m NodeId blk
        , NodeInfo blk (StrictTVar m MockFS)
        )]
    -> m (TestOutput blk)
getTestOutput nodes = do
    nodes' <- fmap Map.fromList $ forM nodes $ \(cid, cfg, node, nodeInfo) -> do
      let chainDB = getChainDB node
      ch <- ChainDB.toChain chainDB
      ChainDB.closeDB chainDB
      nodeInfo' <- readFsTVars nodeInfo
      let nodeOutput = NodeOutput
            { nodeOutputCfg        = cfg
            , nodeOutputFinalChain = ch
            , nodeOutputNodeInfo   = nodeInfo'
            }
      return (fromCoreNodeId cid, nodeOutput)

    pure $ TestOutput
        { testOutputNodes = nodes'
        }

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

-- | Communication channel used for the Chain Sync protocol
type ChainSyncChannel m blk = Channel m (AnyMessage (ChainSync (Header blk) (Point blk)))

-- | Communication channel used for the Block Fetch protocol
type BlockFetchChannel m blk = Channel m (AnyMessage (BlockFetch blk))

-- | Communication channel used for the Tx Submission protocol
type TxSubmissionChannel m blk = Channel m (AnyMessage (TxSubmission (GenTxId blk) (GenTx blk)))

-- | The communication channels from and to each node
data NodeChan m blk = NodeChan
  { chainSyncConsumer    :: ChainSyncChannel    m blk
  , chainSyncProducer    :: ChainSyncChannel    m blk
  , blockFetchConsumer   :: BlockFetchChannel   m blk
  , blockFetchProducer   :: BlockFetchChannel   m blk
  , txSubmissionConsumer :: TxSubmissionChannel m blk
  , txSubmissionProducer :: TxSubmissionChannel m blk
  }

-- | All connections between all nodes
type NodeChans m peer blk = Map peer (Map peer (NodeChan m blk))


{-------------------------------------------------------------------------------
  Constraints needed for verbose tracing
-------------------------------------------------------------------------------}

-- These constraints are when using @showTracer(s) debugTracer@ instead of
-- @nullTracer(s)@.
type TracingConstraints blk =
  ( Show blk
  , Show (Header blk)
  , Show (GenTx blk)
  )
