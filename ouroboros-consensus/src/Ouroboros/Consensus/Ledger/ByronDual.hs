{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ouroboros.Consensus.Ledger.ByronDual (
    DualByronBlock(..)
  , DualByronConfig(..)
  , DualByronConsensusProtocol
  , DualByronLedgerError(..)
  , DualByronTxError(..)
  , concreteByronNodeConfig
  , forgeDualByronBlock
    -- * Data family instances
  , GenTxId(..)
  , Header(..)
  , LedgerConfig(..)
  , LedgerState(..)
    -- * Serialization
  , decodeDualByronApplyTxError
  , decodeDualByronBlock
  , decodeDualByronChainState
  , decodeDualByronGenTx
  , decodeDualByronGenTxId
  , decodeDualByronHeader
  , decodeDualByronHeaderHash
  , decodeDualByronLedgerState
  , encodeDualByronApplyTxError
  , encodeDualByronBlockWithInfo
  , encodeDualByronChainState
  , encodeDualByronGenTx
  , encodeDualByronGenTxId
  , encodeDualByronHeader
  , encodeDualByronHeaderHash
  , encodeDualByronLedgerState
  ) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import           Codec.Serialise
import           Control.Monad.Except
import           Crypto.Random (MonadRandom)
import           Data.Bifunctor
import           Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import qualified Data.ByteString.Lazy as Lazy
import           Data.FingerTree.Strict (Measured (..))
import           Data.Maybe (listToMaybe)
import           Data.Set (Set)
import qualified Data.Set as Set
import           GHC.Generics (Generic)

import           Cardano.Chain.Slotting (EpochSlots (..))
import           Cardano.Prelude (AllowThunk (..), NoUnexpectedThunks)

import qualified Cardano.Ledger.Spec.STS.UTXO as Abstract
import qualified Cardano.Ledger.Spec.STS.UTXOW as Abstract
import qualified Cardano.Spec.Chain.STS.Block as Abstract
import qualified Cardano.Spec.Chain.STS.Rule.Chain as Abstract
import qualified Cardano.Spec.Chain.STS.Rule.Epoch as Abstract
import qualified Control.State.Transition as Abstract
import qualified Ledger.Core as Abstract
import qualified Ledger.Delegation as Abstract
import qualified Ledger.Update as Abstract
import qualified Ledger.UTxO as Abstract

import           Ouroboros.Network.Block

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Byron
import           Ouroboros.Consensus.Mempool.API
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.PBFT

import           Ouroboros.Storage.Common

{-------------------------------------------------------------------------------
  Block
-------------------------------------------------------------------------------}

data DualByronBlock = DualByronBlock {
      concreteByronBlock :: ByronBlock
    , abstractByronBlock :: Abstract.Block
    }

-- The hash is used to identify the block; it suffices to use the hash of
-- the concrete block.
type instance HeaderHash DualByronBlock = HeaderHash ByronBlock

instance StandardHash DualByronBlock

instance Measured BlockMeasure DualByronBlock where
  measure = blockMeasure

instance HasHeader DualByronBlock where
  blockHash      =            blockHash     . getHeader
  blockPrevHash  = castHash . blockPrevHash . getHeader
  blockSlot      =            blockSlot     . getHeader
  blockNo        =            blockNo       . getHeader

  blockInvariant = const True

{-------------------------------------------------------------------------------
  Header

  For the header we /just/ use the concrete header. We send headers around in
  the chain sync client, and we need to be able to update the chain state, but
  we configure PBft to look at the concrete part only /anyway/, so having just
  the concrete header suffices.

  (Having a pair of headers makes the encoding awkward, because the chain DB
  assumes that the header must exist at a  particular offset within the block,
  which wouldn't be true with the default encoding of a pair of blocks.)

  We don't care about unexpected thunks; unexpected thunks in the concrete parts
  are checked elsewhere, and thunks in the abstract parts are unimportant.
-------------------------------------------------------------------------------}

instance GetHeader DualByronBlock where
  newtype Header DualByronBlock = ConcreteByronHeader {
        concreteByronHeader :: Header ByronBlock
      }
    deriving NoUnexpectedThunks via AllowThunk (Header DualByronBlock)

  getHeader DualByronBlock{..} = ConcreteByronHeader {
        concreteByronHeader = getHeader concreteByronBlock
      }

instance HasHeader (Header DualByronBlock) where
  blockHash      =            blockHash     . concreteByronHeader
  blockPrevHash  = castHash . blockPrevHash . concreteByronHeader
  blockSlot      =            blockSlot     . concreteByronHeader
  blockNo        =            blockNo       . concreteByronHeader

  blockInvariant = const True

{-------------------------------------------------------------------------------
  Configuration
-------------------------------------------------------------------------------}

data DualByronConfig = DualByronConfig {
      concreteByronConfig :: ByronConfig
    , abstractByronConfig :: AbstractGenesisConfig
    }
  deriving NoUnexpectedThunks via AllowThunk DualByronConfig

-- | The equivalent of the genesis config for the abstract ledger
data AbstractGenesisConfig = AbstractGenesisConfig {
      -- | Allowed delegators
      -- ('CC.Genesis.configGenesisKeyHashes')
      abstractAllowedDelegators :: Set Abstract.VKeyGenesis

      -- | Initial protocol parameters
    , abstractInitPParams       :: Abstract.PParams

      -- | Initial UTxO
      --
      -- NOTE: This is /not/ the empty UTxO, but must instead contain the
      -- initial balances.
    , abstractInitUTxO          :: Abstract.UTxO

      -- | Security parameter (@k@)
      --
      -- NOTE: spec calls this the "chain stability parameter"
    , abstractSecurityParam     :: Abstract.BlockCount
    }

concreteByronNodeConfig :: NodeConfig DualByronConsensusProtocol
                        -> NodeConfig ByronConsensusProtocol
concreteByronNodeConfig = mapPBftExtConfig concreteByronConfig

{-------------------------------------------------------------------------------
  Protocol

  We only sign (and verify) the concrete part of the block
-------------------------------------------------------------------------------}

type DualByronConsensusProtocol = PBft DualByronConfig PBftCardanoCrypto
type instance BlockProtocol DualByronBlock = DualByronConsensusProtocol

instance HeaderSupportsPBft
           DualByronConfig
           PBftCardanoCrypto
           (Header DualByronBlock) where
  type OptSigned (Header DualByronBlock) = OptSigned (Header ByronBlock)

  headerPBftFields DualByronConfig{..} =
        headerPBftFields concreteByronConfig
      . concreteByronHeader

instance ConstructContextDSIGN DualByronConfig PBftCardanoCrypto where
  constructContextDSIGN _p DualByronConfig{..} genKey = (
        concreteByronConfig
      , genKey
      )

instance SupportedBlock DualByronBlock

{-------------------------------------------------------------------------------
  Ledger
-------------------------------------------------------------------------------}

data DualByronLedgerError =
    -- | Both the concrete ledger and the abstract ledger threw an error
    --
    -- We do not verify that the errors agree, merely that they both report
    -- /some/ error.
    --
    -- If only /one/ of the two semantics reports an error, we fail with an
    -- 'error' (see 'agreeOnError'), rather than a regular chain failure; if
    -- this happens, it indicates a bug, and the node should fail (rather than
    -- just, for example, reject a block).
    DualByronLedgerError {
        concreteByronLedgerError :: LedgerError ByronBlock
      , abstractByronLedgerError :: [[Abstract.PredicateFailure Abstract.CHAIN]]
      }
  deriving stock (Show, Eq)
  deriving NoUnexpectedThunks via AllowThunk DualByronLedgerError

instance UpdateLedger DualByronBlock where

  data LedgerState DualByronBlock = DualByronLedgerState {
        concreteByronLedgerState :: LedgerState ByronBlock
      , abstractByronLedgerState :: Abstract.State Abstract.CHAIN
      }
    deriving stock (Show, Eq)
    deriving NoUnexpectedThunks via AllowThunk (LedgerState DualByronBlock)

  data LedgerConfig DualByronBlock = DualByronLedgerConfig {
        concreteByronLedgerConfig :: LedgerConfig ByronBlock
      , abstractByronLedgerConfig :: AbstractGenesisConfig
      }

  type LedgerError DualByronBlock = DualByronLedgerError

  ledgerConfigView cfg = DualByronLedgerConfig {
        concreteByronLedgerConfig = ledgerConfigView $
                                      concreteByronNodeConfig cfg
      , abstractByronLedgerConfig = abstractByronConfig $
                                      pbftExtConfig cfg
      }

  applyChainTick DualByronLedgerConfig{..}
                 (SlotNo slot)
                 DualByronLedgerState{..} =
      TickedLedgerState DualByronLedgerState {
          concreteByronLedgerState = getTickedLedgerState $
            applyChainTick
              concreteByronLedgerConfig
              (SlotNo slot)
              concreteByronLedgerState
        , abstractByronLedgerState =
            applyAbstractChainTick
              abstractByronLedgerConfig
              (Abstract.Slot slot)
              abstractByronLedgerState
        }

  applyLedgerBlock DualByronLedgerConfig{..}
                   DualByronBlock{..}
                   DualByronLedgerState{..} = do
      (concrete', abstract') <-
        agreeOnError DualByronLedgerError
          ( runExcept $ applyLedgerBlock
                concreteByronLedgerConfig
                concreteByronBlock
                concreteByronLedgerState
          , Abstract.applySTS @Abstract.CHAIN $ Abstract.TRC (
                stsChainEnvironment abstractByronLedgerConfig
              , abstractByronLedgerState
              , abstractByronBlock
              )
          )
      return $ DualByronLedgerState concrete' abstract'

  reapplyLedgerBlock DualByronLedgerConfig{..}
                     DualByronBlock{..}
                     DualByronLedgerState{..} =
      -- It's good to test the normal code path here for the concrete ledger,
      -- even if the abstract ledger doesn't provide this functionaltiy
      case ( reapplyLedgerBlock
                 concreteByronLedgerConfig
                 concreteByronBlock
                 concreteByronLedgerState
           , Abstract.applySTS @Abstract.CHAIN $ Abstract.TRC (
                 stsChainEnvironment abstractByronLedgerConfig
               , abstractByronLedgerState
               , abstractByronBlock
               )
           ) of
        (_, Left _) ->
          error "reapplyLedgerBlock: impossible"
        (concrete', Right abstract') ->
          DualByronLedgerState concrete' abstract'

  ledgerTipPoint = castPoint . ledgerTipPoint . concreteByronLedgerState

-- For the protocol view, we just use the state from the concrete block
instance ProtocolLedgerView DualByronBlock where
  protocolLedgerView cfg state =
      protocolLedgerView
        (concreteByronNodeConfig  cfg)
        (concreteByronLedgerState state)

  anachronisticProtocolLedgerView cfg state =
      anachronisticProtocolLedgerView
        (concreteByronNodeConfig  cfg)
        (concreteByronLedgerState state)

{-------------------------------------------------------------------------------
  Chain tick
-------------------------------------------------------------------------------}

-- | Chain tick
--
-- There isn't something in the spec that directly corresponds to this, so we
-- have to combine a few different things:
--
-- 1. Apply the epoch rule (update the update state)
-- 2. Apply any scheduled delegations
-- 3. Set the slot number
--
-- This matches quite closely what 'applyChainTick' in
-- "Ouroboros.Consensus.Ledger.Byron.Auxiliary" does.
applyAbstractChainTick :: AbstractGenesisConfig
                       -> Abstract.Slot
                       -> Abstract.State Abstract.CHAIN
                       -> Abstract.State Abstract.CHAIN
applyAbstractChainTick cfg slot st =
    case (Abstract.applySTS epoch, Abstract.applySTS deleg) of
      (Left _, _) ->
        error "applyAbstractChainTick: unexpected EPOCH failure"
      (_, Left _) ->
        error "applyAbstractChainTick: unexpected DELEG failure"
      (Right updateState', Right delegState') ->
          setSlot     slot
        . setUPIState updateState'
        . setDIState  delegState'
        $ st
  where
    epoch :: Abstract.RuleContext Abstract.Transition Abstract.EPOCH
    epoch = Abstract.TRC (envEPOCH cfg st, getUPIState st, slot)

    -- Empty list of certificates; we apply the rule only to apply previously
    -- scheduled delegations (rather than to introduce new ones)
    deleg :: Abstract.RuleContext Abstract.Transition Abstract.DELEG
    deleg = Abstract.TRC (envDELEG cfg st, getDIState st, [])

{-------------------------------------------------------------------------------
  Generalized abstract transactions
-------------------------------------------------------------------------------}

-- | Generalized transaction
--
-- The spec doesn't have a type for this, instead splitting the block body
-- into separate lists
data AbstractGenTx =
    AbstractDCert Abstract.DCert
  | AbstractTx    Abstract.TxWits
  | AbstractUProp Abstract.UProp
  | AbstractVote  Abstract.Vote
  deriving (Generic, Serialise)

data AbstractGenTxErr =
    AbstractErrDCert  [[Abstract.PredicateFailure Abstract.SDELEG]]
  | AbstractErrTx     [[Abstract.PredicateFailure Abstract.UTXOW]]
  | AbstractErrUProp  [[Abstract.PredicateFailure Abstract.UPIREG]]
  | AbstractErrVote   [[Abstract.PredicateFailure Abstract.UPIVOTE]]
  deriving (Show, Generic, Serialise)

applyAbstractGenTx :: AbstractGenesisConfig
                   -> AbstractGenTx
                   -> Abstract.State Abstract.CHAIN
                   -> Either AbstractGenTxErr (Abstract.State Abstract.CHAIN)
applyAbstractGenTx cfg genTx st =
    case genTx of
      AbstractDCert dcert ->
        bimap AbstractErrDCert (`setDSState` st) $
          Abstract.applySTS @Abstract.SDELEG $
            Abstract.TRC (envSDELEG cfg st, getDSState st, dcert)
      AbstractTx tx ->
        bimap AbstractErrTx (`setUtxoState` st) $
          Abstract.applySTS @Abstract.UTXOW $
            Abstract.TRC (envUTXOW cfg st, getUtxoState st, tx)
      AbstractUProp prop ->
        bimap AbstractErrUProp (`setUPIState` st) $
          Abstract.applySTS @Abstract.UPIREG $
            Abstract.TRC (envUPIREG cfg st, getUPIState st, prop)
      AbstractVote vote ->
        bimap AbstractErrVote (`setUPIState` st) $
          Abstract.applySTS @Abstract.UPIVOTE $
            Abstract.TRC (envUPIVOTE cfg st, getUPIState st, vote)

partitionAbstractGenTx :: [AbstractGenTx]
                       -> ( [Abstract.DCert]
                          , [Abstract.TxWits]
                          , [Abstract.UProp]
                          , [Abstract.Vote]
                          )
partitionAbstractGenTx = go ([], [], [], [])
  where
    go (ds, ts, us, vs) [] = (reverse ds, reverse ts, reverse us, reverse vs)
    go (ds, ts, us, vs) (g:gs) =
        case g of
          AbstractDCert d -> go (d:ds,   ts,   us,   vs) gs
          AbstractTx    t -> go (  ds, t:ts,   us,   vs) gs
          AbstractUProp u -> go (  ds,   ts, u:us,   vs) gs
          AbstractVote  v -> go (  ds,   ts,   us, v:vs) gs

{-------------------------------------------------------------------------------
  Mempool
-------------------------------------------------------------------------------}

data DualByronTxError = DualByronTxError {
      concreteByronTxError :: ApplyTxErr ByronBlock
    , abstractByronTxError :: AbstractGenTxErr
    }

instance ApplyTx DualByronBlock where
  data GenTx DualByronBlock = DualByronGenTx {
        concreteByronGenTx :: GenTx ByronBlock
      , abstractByronGenTx :: AbstractGenTx
      }
    deriving NoUnexpectedThunks via AllowThunk (GenTx DualByronBlock)

  -- We don't need a pair of IDs, as long as we can unique ID the transaction
  newtype GenTxId DualByronBlock = ConcreteByronGenTxId {
        concreteByronGenTxId :: GenTxId ByronBlock
      }
    deriving (Eq, Ord)

  type ApplyTxErr DualByronBlock = DualByronTxError

  txId = ConcreteByronGenTxId . txId . concreteByronGenTx

  -- We don't really care too much about transaction size, but we shouldn't
  -- exceed max block size for the real block. So we just report the size
  -- of the concrete transaction.
  txSize = txSize . concreteByronGenTx

  applyTx DualByronLedgerConfig{..}
          DualByronGenTx{..}
          (TickedLedgerState DualByronLedgerState{..}) = do
      (TickedLedgerState concrete', abstract') <-
        agreeOnError DualByronTxError
          ( runExcept $ applyTx
              concreteByronLedgerConfig
              concreteByronGenTx
              (TickedLedgerState concreteByronLedgerState)
          , applyAbstractGenTx
              abstractByronLedgerConfig
              abstractByronGenTx
              abstractByronLedgerState
          )
      return $ TickedLedgerState $
        DualByronLedgerState concrete' abstract'

  reapplyTx DualByronLedgerConfig{..}
            DualByronGenTx{..}
            (TickedLedgerState DualByronLedgerState{..}) = do
      (TickedLedgerState concrete', abstract') <-
        agreeOnError DualByronTxError
          ( runExcept $ reapplyTx
              concreteByronLedgerConfig
              concreteByronGenTx
              (TickedLedgerState concreteByronLedgerState)
          , applyAbstractGenTx
              abstractByronLedgerConfig
              abstractByronGenTx
              abstractByronLedgerState
          )
      return $ TickedLedgerState $
        DualByronLedgerState concrete' abstract'

  reapplyTxSameState DualByronLedgerConfig{..}
                     DualByronGenTx{..}
                     (TickedLedgerState DualByronLedgerState{..}) = do
      case ( reapplyTxSameState
               concreteByronLedgerConfig
               concreteByronGenTx
               (TickedLedgerState concreteByronLedgerState)
           , applyAbstractGenTx
               abstractByronLedgerConfig
               abstractByronGenTx
               abstractByronLedgerState
           ) of
        (_, Left _) ->
          error "reapplyTxSameState: impossible"
        (TickedLedgerState concrete', Right abstract') ->
          TickedLedgerState $
            DualByronLedgerState concrete' abstract'

{-------------------------------------------------------------------------------
  Environment for the top-level chain rule

  Unlike the rules for individual transactions (below), this is just constructed
  from the genesis config, and does not depend on state.
-------------------------------------------------------------------------------}

stsChainEnvironment :: AbstractGenesisConfig
                    -> Abstract.Environment Abstract.CHAIN
stsChainEnvironment genesisConfig = (
      -- Used only to check for blocks in the future
      -- (Needed because the block transition does the PBFT transition)
      Abstract.Slot maxBound
      -- The rest of the parameters just come from the (abstract) genesis config
    , abstractInitUTxO          genesisConfig
    , abstractAllowedDelegators genesisConfig
    , abstractInitPParams       genesisConfig
    , abstractSecurityParam     genesisConfig
    )

{-------------------------------------------------------------------------------
  Auxiliary: environments to apply SOS rules

  NOTE: These environments pull in some information from the (abstract) genesis
  config and some information from the state; the reason is that although some
  values come from the state, as far as these rules are concerned, they are
  constants.
-------------------------------------------------------------------------------}

type ConstructEnv sts = AbstractGenesisConfig
                     -> Abstract.State Abstract.CHAIN
                     -> Abstract.Environment sts

envUTXOW :: ConstructEnv Abstract.UTXOW
envUTXOW AbstractGenesisConfig{..} st = Abstract.UTxOEnv {
      utxo0 = abstractInitUTxO
    , pps   = Abstract.protocolParameters (getUPIState st)
    }

envEPOCH :: ConstructEnv Abstract.EPOCH
envEPOCH AbstractGenesisConfig{..} st = (
      -- The _current_ epoch
      -- This is needed to detect if the new slot introduces the next epoch
      Abstract.sEpoch (getSlot st) abstractSecurityParam
    , abstractSecurityParam
    )

envDELEG :: ConstructEnv Abstract.DELEG
envDELEG AbstractGenesisConfig{..} st = Abstract.DSEnv {
      _dSEnvAllowedDelegators = abstractAllowedDelegators
    , _dSEnvEpoch = Abstract.sEpoch (getSlot st) abstractSecurityParam
    , _dSEnvSlot  = getSlot st
    , _dSEnvK     = abstractSecurityParam
    }

envSDELEG :: ConstructEnv Abstract.SDELEG
envSDELEG = envDELEG

envUPIREG :: ConstructEnv Abstract.UPIREG
envUPIREG AbstractGenesisConfig{..} st = (
      getSlot st
    , Abstract._dIStateDelegationMap (getDIState st)
    , abstractSecurityParam
    , fromIntegral $ Set.size abstractAllowedDelegators
    )

envUPIVOTE :: ConstructEnv Abstract.UPIVOTE
envUPIVOTE = envUPIREG

{-------------------------------------------------------------------------------
  Block forging
-------------------------------------------------------------------------------}

forgeDualByronBlock
  :: forall m.
     ( HasNodeState_ () m  -- @()@ is the @NodeState@ of PBFT
     , MonadRandom m
     )
  => NodeConfig DualByronConsensusProtocol
  -> SlotNo                          -- ^ Current slot
  -> BlockNo                         -- ^ Current block number
  -> ChainHash DualByronBlock        -- ^ Previous hash
  -> [GenTx DualByronBlock]          -- ^ Txs to add in the block
  -> PBftIsLeader PBftCardanoCrypto  -- ^ Leader proof ('IsLeader')
  -> m DualByronBlock
forgeDualByronBlock cfg (SlotNo slot) block hash txs isLeader = do
    concreteByronBlock <- forgeByronBlock
                            (concreteByronNodeConfig cfg)
                            (SlotNo slot)
                            block
                            (castHash hash)
                            (map concreteByronGenTx txs)
                            isLeader

    let (ds, ts, us, vs)   = partitionAbstractGenTx (map abstractByronGenTx txs)
        abstractByronBlock = Abstract.mkBlock
                               (_ hash)
                               (Abstract.Slot slot)
                               (_ isLeader)
                               _
                               ds
                               (listToMaybe us)
                               vs
                               ts

    return DualByronBlock{..}

{-------------------------------------------------------------------------------
  Auxiliary: lenses into the abstract chain state
-------------------------------------------------------------------------------}

getSlot :: Abstract.State Abstract.CHAIN
        -> Abstract.Slot
getSlot (a, _, _, _, _, _) = a

setSlot :: Abstract.Slot
        -> Abstract.State Abstract.CHAIN
        -> Abstract.State Abstract.CHAIN
setSlot a (_, b, c, d, e, f) = (a, b, c, d, e, f)

getUtxoState :: Abstract.State Abstract.CHAIN
             -> Abstract.UTxOState
getUtxoState (_, _, _, d, _, _) = d

setUtxoState :: Abstract.UTxOState
             -> Abstract.State Abstract.CHAIN
             -> Abstract.State Abstract.CHAIN
setUtxoState d (a, b, c, _, e, f) = (a, b, c, d, e, f)

getDIState :: Abstract.State Abstract.CHAIN -> Abstract.DIState
getDIState (_, _, _, _, e, _) = e

setDIState :: Abstract.DIState
           -> Abstract.State Abstract.CHAIN
           -> Abstract.State Abstract.CHAIN
setDIState e (a, b, c, d, _, f) = (a, b, c, d, e, f)

-- There is a lens in Ledger.Delegation to do this but we are phasing out
-- @lens@ across all repos, so don't want to depend on it here
getDSState :: Abstract.State Abstract.CHAIN -> Abstract.DSState
getDSState st =
    Abstract.DSState
      _dIStateScheduledDelegations
      _dIStateKeyEpochDelegations
  where
    Abstract.DIState{..} = getDIState st

-- See comment about @lens@ for 'getDSState'
setDSState :: Abstract.DSState
           -> Abstract.State Abstract.CHAIN
           -> Abstract.State Abstract.CHAIN
setDSState Abstract.DSState{..} st =
    setDIState (updateDIState (getDIState st)) st
  where
    updateDIState :: Abstract.DIState -> Abstract.DIState
    updateDIState diState = Abstract.DIState
      (Abstract._dIStateDelegationMap  diState)
      (Abstract._dIStateLastDelegation diState)
      _dSStateScheduledDelegations
      _dSStateKeyEpochDelegations

getUPIState :: Abstract.State Abstract.CHAIN
               -> Abstract.UPIState
getUPIState (_, _, _, _, _, f) = f

setUPIState :: Abstract.UPIState
            -> Abstract.State Abstract.CHAIN
            -> Abstract.State Abstract.CHAIN
setUPIState f (a, b, c, d, e, _) = (a, b, c, d, e, f)

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | Used when the concrete and abstract implementation should agree on errors
--
-- The abstract-versus-concrete tests from the ledger folks tests precisely
-- this, so if this fails, it indicates a bug somewhere and we error out.
agreeOnError :: (MonadError err m, Show e, Show e')
             => (e -> e' -> err)
             -> (Either e a, Either e' b)
             -> m (a, b)
agreeOnError f (Left e, Left e') =
    throwError $ f e e'
agreeOnError _ (Left e, Right _) =
    error $ "agreeOnError: unexpected error " ++ show e
agreeOnError _ (Right _, Left e') =
    error $ "agreeOnError: unexpected error " ++ show e'
agreeOnError _ (Right a, Right b) =
    return (a, b)

{-------------------------------------------------------------------------------
  Serialization

  In all cases, we serialise the concrete part before the abstract part. This
  is important for two reasons:

  * In order for slicing to work during deserialization, we must make sure to
    encode the concrete thing before the abstract thing, so that when the
    decoder slices the bytestring, the offsets still make sense, even if the
    bytestring it's slicing from is now longer.
  * Similarly, the 'BinaryInfo' for a block records the offset of the header.
    If we encode the concrete block first, then the offset of the (concrete)
    header stays the same (we don't extract the abstract header at all).
-------------------------------------------------------------------------------}

encodeDualByronHeader :: Header DualByronBlock -> Encoding
encodeDualByronHeader ConcreteByronHeader{..} = mconcat [
      encodeByronHeader concreteByronHeader
    ]

decodeDualByronHeader :: EpochSlots
                      -> Decoder s (Lazy.ByteString -> Header DualByronBlock)
decodeDualByronHeader epochSlots =
    dualByronHeader
      <$> decodeByronHeader epochSlots
  where
    dualByronHeader :: (Lazy.ByteString -> Header ByronBlock)
                    -> (Lazy.ByteString -> Header DualByronBlock)
    dualByronHeader conc bs = ConcreteByronHeader (conc bs)

-- We override the encoding, but leave the length and offset of the header
encodeDualByronBlockWithInfo :: DualByronBlock -> BinaryInfo Encoding
encodeDualByronBlockWithInfo DualByronBlock{..} =
    (encodeByronBlockWithInfo concreteByronBlock) {
        binaryBlob = mconcat [
            encodeByronBlock concreteByronBlock
          , encode           abstractByronBlock
          ]
      }

decodeDualByronBlock :: EpochSlots
                     -> Decoder s (Lazy.ByteString -> DualByronBlock)
decodeDualByronBlock epochSlots =
    dualByronBlock
      <$> decodeByronBlock epochSlots
      <*> decode
  where
    dualByronBlock :: (Lazy.ByteString -> ByronBlock)
                   -> Abstract.Block
                   -> (Lazy.ByteString -> DualByronBlock)
    dualByronBlock conc abst bs = DualByronBlock (conc bs) abst

encodeDualByronGenTxId :: GenTxId DualByronBlock -> Encoding
encodeDualByronGenTxId = encodeByronGenTxId . concreteByronGenTxId

decodeDualByronGenTxId :: Decoder s (GenTxId DualByronBlock)
decodeDualByronGenTxId = ConcreteByronGenTxId <$> decodeByronGenTxId

encodeDualByronHeaderHash :: HeaderHash DualByronBlock -> Encoding
encodeDualByronHeaderHash = encodeByronHeaderHash

decodeDualByronHeaderHash :: Decoder s (HeaderHash DualByronBlock)
decodeDualByronHeaderHash = decodeByronHeaderHash

encodeDualByronLedgerState :: LedgerState DualByronBlock -> Encoding
encodeDualByronLedgerState DualByronLedgerState{..} = mconcat [
      encodeByronLedgerState concreteByronLedgerState
    , encode                 abstractByronLedgerState
    ]

decodeDualByronLedgerState :: Decoder s (LedgerState DualByronBlock)
decodeDualByronLedgerState =
    DualByronLedgerState
      <$> decodeByronLedgerState
      <*> decode

encodeDualByronGenTx :: GenTx DualByronBlock -> Encoding
encodeDualByronGenTx DualByronGenTx{..} = mconcat [
      encodeByronGenTx concreteByronGenTx
    , encode           abstractByronGenTx
    ]

decodeDualByronGenTx :: Decoder s (GenTx DualByronBlock)
decodeDualByronGenTx =
    DualByronGenTx
      <$> decodeByronGenTx
      <*> decode

encodeDualByronApplyTxError :: ApplyTxErr DualByronBlock -> Encoding
encodeDualByronApplyTxError DualByronTxError{..} = mconcat [
      encodeByronApplyTxError concreteByronTxError
    , encode                  abstractByronTxError
    ]

decodeDualByronApplyTxError :: Decoder s (ApplyTxErr DualByronBlock)
decodeDualByronApplyTxError =
    DualByronTxError
      <$> decodeByronApplyTxError
      <*> decode

encodeDualByronChainState :: ChainState DualByronConsensusProtocol -> Encoding
encodeDualByronChainState = encodeByronChainState

decodeDualByronChainState :: SecurityParam
                          -> Decoder s (ChainState DualByronConsensusProtocol)
decodeDualByronChainState = decodeByronChainState

{-------------------------------------------------------------------------------
  Orphan instances

  We use generic serializers for the abstract spec.
-------------------------------------------------------------------------------}

instance Serialise Abstract.Addr
instance Serialise Abstract.ApName
instance Serialise Abstract.ApVer
instance Serialise Abstract.BkSgnCntT
instance Serialise Abstract.Block
instance Serialise Abstract.BlockBody
instance Serialise Abstract.BlockHeader
instance Serialise Abstract.DCert
instance Serialise Abstract.DIState
instance Serialise Abstract.Epoch
instance Serialise Abstract.EpochDiff
instance Serialise Abstract.FactorA
instance Serialise Abstract.FactorB
instance Serialise Abstract.Hash
instance Serialise Abstract.Lovelace
instance Serialise Abstract.Metadata
instance Serialise Abstract.Owner
instance Serialise Abstract.PParams
instance Serialise Abstract.ProtVer
instance Serialise Abstract.Slot
instance Serialise Abstract.SlotCount
instance Serialise Abstract.SwVer
instance Serialise Abstract.Tx
instance Serialise Abstract.TxId
instance Serialise Abstract.TxIn
instance Serialise Abstract.TxOut
instance Serialise Abstract.TxWits
instance Serialise Abstract.UpAdptThd
instance Serialise Abstract.UpdateConstraintViolation
instance Serialise Abstract.UpId
instance Serialise Abstract.UProp
instance Serialise Abstract.UTxO
instance Serialise Abstract.UTxOState
instance Serialise Abstract.VKey
instance Serialise Abstract.VKeyGenesis
instance Serialise Abstract.Vote
instance Serialise Abstract.Wit

instance Serialise (Abstract.PredicateFailure Abstract.ADDVOTE)
instance Serialise (Abstract.PredicateFailure Abstract.SDELEG)
instance Serialise (Abstract.PredicateFailure Abstract.UPIREG)
instance Serialise (Abstract.PredicateFailure Abstract.UPIVOTE)
instance Serialise (Abstract.PredicateFailure Abstract.UPPVV)
instance Serialise (Abstract.PredicateFailure Abstract.UPREG)
instance Serialise (Abstract.PredicateFailure Abstract.UPSVV)
instance Serialise (Abstract.PredicateFailure Abstract.UPV)
instance Serialise (Abstract.PredicateFailure Abstract.UPVOTE)
instance Serialise (Abstract.PredicateFailure Abstract.UTXO)
instance Serialise (Abstract.PredicateFailure Abstract.UTXOW)

instance Serialise a => Serialise (Abstract.Sig       a)
instance Serialise a => Serialise (Abstract.Threshold a)

{-------------------------------------------------------------------------------
  Not all types in cardano-ledger-specs have generic instances
-------------------------------------------------------------------------------}

deriving instance Generic Abstract.DIState
deriving instance Generic Abstract.EpochDiff
deriving instance Generic Abstract.UpdateConstraintViolation
deriving instance Generic Abstract.UTxO
deriving instance Generic Abstract.UTxOState

deriving instance Generic (Abstract.Threshold a)

deriving instance Generic (Abstract.PredicateFailure Abstract.ADDVOTE)
deriving instance Generic (Abstract.PredicateFailure Abstract.SDELEG)
deriving instance Generic (Abstract.PredicateFailure Abstract.UPIREG)
deriving instance Generic (Abstract.PredicateFailure Abstract.UPIVOTE)
deriving instance Generic (Abstract.PredicateFailure Abstract.UPPVV)
deriving instance Generic (Abstract.PredicateFailure Abstract.UPREG)
deriving instance Generic (Abstract.PredicateFailure Abstract.UPSVV)
deriving instance Generic (Abstract.PredicateFailure Abstract.UPV)
deriving instance Generic (Abstract.PredicateFailure Abstract.UPVOTE)
deriving instance Generic (Abstract.PredicateFailure Abstract.UTXO)
deriving instance Generic (Abstract.PredicateFailure Abstract.UTXOW)

{-------------------------------------------------------------------------------
  Orphans for generic types

  TODO: Unlike the spec types above, this could actually lead to incoherence :/
-------------------------------------------------------------------------------}

instance ( Ord k, Ord v
         , Serialise k, Serialise v
         ) => Serialise (Bimap k v) where
  encode = encode . Bimap.toList
  decode = Bimap.fromList <$> decode
