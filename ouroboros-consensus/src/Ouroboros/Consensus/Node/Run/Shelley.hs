module Ouroboros.Consensus.Node.Run.Shelley () where

import           Cardano.Binary (fromCBOR, toCBOR)
import           Cardano.Slotting.EpochInfo
import           Data.Functor.Identity (runIdentity)
import           Ouroboros.Consensus.Ledger.Shelley
import           Ouroboros.Consensus.Ledger.Shelley.Block (shelleyHashInfo)
import           Ouroboros.Consensus.Node.Run.Abstract
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.TPraos

instance RunNode ShelleyBlock where
  nodeForgeBlock = forgeShelleyBlock
  nodeBlockMatchesHeader = undefined
  nodeBlockFetchSize = const 2000 -- TODO
  nodeIsEBB = const Nothing

  nodeEpochSize = \_proxy cfg epochNo ->
    pure . runIdentity $
      epochInfoSize
        ( tpraosEpochInfo $
            tpraosParams cfg
        )
        epochNo

  nodeStartTime =
    const $
      sncStartTime
        . tpraosExtraConfig
  nodeNetworkMagic =
    const $
      sncNetworkMagic
        . tpraosExtraConfig
  nodeProtocolMagicId =
    const $
      sncProtocolMagicId
        . tpraosExtraConfig

  nodeHashInfo = const shelleyHashInfo

  nodeEncodeBlockWithInfo = undefined -- TODO
  nodeEncodeHeader = const toCBOR
  nodeEncodeGenTx = toCBOR
  nodeEncodeGenTxId = toCBOR
  nodeEncodeHeaderHash = const toCBOR
  nodeEncodeLedgerState = const encodeShelleyLedgerState
  nodeEncodeChainState = \_proxy _cfg -> toCBOR
  nodeEncodeApplyTxError = const toCBOR

  nodeDecodeBlock = const fromCBOR
  nodeDecodeHeader = const fromCBOR
  nodeDecodeGenTx = fromCBOR
  nodeDecodeGenTxId = fromCBOR
  nodeDecodeHeaderHash = const fromCBOR
  nodeDecodeLedgerState = const decodeShelleyLedgerState
  nodeDecodeChainState = \_proxy cfg -> fromCBOR
  nodeDecodeApplyTxError = const fromCBOR
