{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTSyntax                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

module Ouroboros.Consensus.Ledger.Shelley
  ()
where

import qualified BaseTypes as SL
import           BlockChain (BHBody (..), BHeader (..), Block (..), HashHeader,
                     bhHash, bhbody, bheader)
import           Cardano.Binary (ToCBOR (..))
import           Cardano.Ledger.Shelley.API
import           Cardano.Prelude (NoUnexpectedThunks (..))
import           Cardano.Slotting.Slot (WithOrigin (..), fromWithOrigin)
import           Control.Arrow (left)
import           Control.Monad.Except (ExceptT (..), runExcept, withExcept)
import           Control.Monad.Identity (Identity (..))
import           Data.Coerce (coerce)
import           Data.Either (fromRight)
import           Data.FingerTree.Strict (Measured (..))
import           Data.Proxy (Proxy (..))
import           Data.Typeable (typeRep)
import           GHC.Generics (Generic)
import qualified LedgerState as SL
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Abstract
import qualified Ouroboros.Consensus.Ledger.Shelley.History as History
import           Ouroboros.Consensus.Protocol.Abstract (SecurityParam (..))
import           Ouroboros.Consensus.Protocol.Signed
import           Ouroboros.Consensus.Protocol.TPraos
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Network.Block

{-------------------------------------------------------------------------------
  Header hash
-------------------------------------------------------------------------------}

newtype ShelleyHash = ShelleyHash { unShelleyHash :: HashHeader TPraosStandardCrypto }
  deriving stock   (Eq, Ord, Show, Generic)
  deriving newtype (ToCBOR) -- TODO , FromCBOR)
  deriving anyclass NoUnexpectedThunks

instance Condense ShelleyHash where
  condense = show . unShelleyHash

{-------------------------------------------------------------------------------
  Shelley blocks and headers
-------------------------------------------------------------------------------}

-- | Newtype wrapper to avoid orphan instances
--
-- The phantom type parameter is there to record the additional information
-- we need to work with this block. Most of the code here does not care,
-- but we may need different additional information when running the chain.
newtype ShelleyBlock = ShelleyBlock
  { unShelleyBlock :: Block TPraosStandardCrypto
  }
  deriving (Eq, Show)

instance GetHeader ShelleyBlock where
  data Header ShelleyBlock = ShelleyHeader
    { shelleyHeader :: !(BHeader TPraosStandardCrypto)
      -- Cached hash
    , shelleyHeaderHash :: ShelleyHash
    } deriving (Eq, Show)

  getHeader (ShelleyBlock b) = ShelleyHeader
    { shelleyHeader     = bheader b
    , shelleyHeaderHash = ShelleyHash . bhHash . bheader $ b
    }

instance NoUnexpectedThunks (Header ShelleyBlock) where
  showTypeOf _ = show $ typeRep (Proxy @(Header ShelleyBlock ))

-- We explicitly allow the hash to be a thunk
  whnfNoUnexpectedThunks ctxt (ShelleyHeader hdr _hash) =
    noUnexpectedThunks ctxt hdr

instance SupportedBlock ShelleyBlock

type instance HeaderHash ShelleyBlock = ShelleyHash

instance HasHeader ShelleyBlock  where
  blockHash      = blockHash . getHeader
  blockPrevHash  = castHash . blockPrevHash . getHeader
  blockSlot      = blockSlot . getHeader
  blockNo        = blockNo . getHeader
  blockInvariant = const True

instance HasHeader (Header ShelleyBlock) where
  blockHash = shelleyHeaderHash

  blockPrevHash =
    BlockHash . ShelleyHash . bheaderPrev . bhbody . shelleyHeader

  blockSlot      = bheaderSlotNo . bhbody . shelleyHeader
  blockNo        = coerce . bheaderBlockNo . bhbody . shelleyHeader
  blockInvariant = const True

instance Measured BlockMeasure ShelleyBlock where
  measure = blockMeasure

instance StandardHash ShelleyBlock

{-------------------------------------------------------------------------------
  Ledger
-------------------------------------------------------------------------------}

-- | See note on @CombinedLedgerState@
data CombinedLedgerError =
    TickError (TickTransitionError TPraosStandardCrypto)
  | BBodyError (BlockTransitionError TPraosStandardCrypto)
  deriving (Eq, Generic, Show)

instance NoUnexpectedThunks CombinedLedgerError

instance UpdateLedger ShelleyBlock where

  data LedgerState ShelleyBlock = ShelleyLedgerState
    { ledgerTip :: Point ShelleyBlock
    , history :: History.LedgerViewHistory
    , shelleyLedgerState :: SL.NewEpochState TPraosStandardCrypto
    } deriving (Eq, Show, Generic)
  type LedgerError ShelleyBlock = CombinedLedgerError

  newtype LedgerConfig ShelleyBlock = ShelleyLedgerConfig SL.Globals
    deriving Generic

  ledgerConfigView TPraosNodeConfig { tpraosParams } =
    ShelleyLedgerConfig $ SL.Globals
      { SL.epochInfo = tpraosEpochInfo tpraosParams
      , SL.slotsPerKESPeriod = tpraosKESPeriod tpraosParams
      }

  applyChainTick
    (ShelleyLedgerConfig globals)
    slotNo
    (ShelleyLedgerState pt history bhState)
    = TickedLedgerState
    . ShelleyLedgerState pt history
    $ applyTickTransition globals bhState slotNo

  applyLedgerBlock (ShelleyLedgerConfig globals)
        sb@(ShelleyBlock blk) (ShelleyLedgerState _ history bhState)
    = do
        st' <- withExcept BBodyError $ applyBlockTransition globals bhState blk
        let history' =
              if currentLedgerView bhState == currentLedgerView st'
              then history
              else History.snapOld
                    undefined -- TODO security parameter
                    (blockSlot sb)
                    (currentLedgerView bhState)
                    history

        pure $! ShelleyLedgerState newPt history' st'
      where
        newPt
          = BlockPoint
              (bheaderSlotNo . bhbody $ bheader blk)
              (ShelleyHash . bhHash $ bheader blk)

  -- TODO actual reapplication
  reapplyLedgerBlock cfg blk ls =
    let ExceptT (Identity e) = applyLedgerBlock cfg blk ls
        err = error "Panic! Reapplication of Shelley ledger block failed"
    in fromRight err e

  ledgerTipPoint = ledgerTip

instance NoUnexpectedThunks (LedgerState ShelleyBlock)

{-------------------------------------------------------------------------------
  Support for Praos consensus algorithm
-------------------------------------------------------------------------------}

type instance BlockProtocol ShelleyBlock
  = TPraos TPraosStandardCrypto

instance SignedHeader (Header ShelleyBlock) where
  type Signed (Header ShelleyBlock) = BHBody TPraosStandardCrypto
  headerSigned = bhbody . shelleyHeader

instance HeaderSupportsTPraos TPraosStandardCrypto (Header ShelleyBlock) where
  headerToBHeader _ (ShelleyHeader hdr _hash) = hdr

instance ProtocolLedgerView ShelleyBlock where
  protocolLedgerView _cfg (ShelleyLedgerState _ _ ls) = currentLedgerView ls

  anachronisticProtocolLedgerView
    cfg
    (ShelleyLedgerState
      (fromWithOrigin genesisSlotNo . pointSlot -> now)
      history
      st
    )
    woslot = case History.find woslot history of
        Just lv -> Right lv
        Nothing
          | woslot <  At maxLo -> Left TooFarBehind -- lower bound is inclusive
          | woslot >= At maxHi -> Left TooFarAhead  -- upper bound is exclusive
          | otherwise        ->
              left (const TooFarAhead) -- TODO maybe incorrect,
                                       -- but we don't expect an error here
                . runExcept
                $ futureLedgerView globals st
                    (fromWithOrigin genesisSlotNo woslot)
      where
        SecurityParam k = tpraosSecurityParam . tpraosParams $ cfg

        ShelleyLedgerConfig globals = ledgerConfigView cfg

        maxHi, maxLo :: SlotNo
        maxLo = SlotNo $ if (2 * k) > unSlotNo now
                          then 0
                          else unSlotNo now - (2 * k)
        maxHi = SlotNo $ unSlotNo now + (2 * k)
