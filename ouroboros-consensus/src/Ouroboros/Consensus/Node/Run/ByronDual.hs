{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ouroboros.Consensus.Node.Run.ByronDual () where

import           Data.Proxy

import           Cardano.Chain.Slotting (EpochSlots (..))

import           Ouroboros.Consensus.Ledger.Byron
import           Ouroboros.Consensus.Ledger.ByronDual
import           Ouroboros.Consensus.Node.Run.Abstract
import qualified Ouroboros.Consensus.Node.Run.Byron as Byron
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.PBFT

pb :: Proxy ByronBlock
pb = Proxy

instance RunNode DualByronBlock where
  nodeForgeBlock = forgeDualByronBlock

  -- Functions that extract data from the config can just delegate to the
  -- concrete block. /Where/ these values are also relevant to the abstract
  -- spec, we should anyway make sure that the concrete and abstract setup
  -- is the same.
  nodeEpochSize       = \_p -> nodeEpochSize       pb . concreteByronNodeConfig
  nodeStartTime       = \_p -> nodeStartTime       pb . concreteByronNodeConfig
  nodeNetworkMagic    = \_p -> nodeNetworkMagic    pb . concreteByronNodeConfig
  nodeProtocolMagicId = \_p -> nodeProtocolMagicId pb . concreteByronNodeConfig

  -- The hash we use is the hash of the concrete block
  nodeHashInfo = \_p -> nodeHashInfo pb

  -- We can look at the concrete header to see if this is an EBB
  nodeIsEBB = nodeIsEBB . concreteByronHeader

  -- For now the size of the block is just an estimate, and so we just reuse
  -- the estimate from the concrete header. Once we instantiate BF with a pair
  -- of a header and a size, this will change.
  nodeBlockFetchSize = nodeBlockFetchSize . concreteByronHeader

  -- We don't really care too much about data loss or malicious behaviour for
  -- the dual ledger tests, so integrity and match checks can just use the
  -- concrete implementation
  nodeBlockMatchesHeader hdr blk =
      nodeBlockMatchesHeader (concreteByronHeader hdr) (concreteByronBlock blk)
  nodeCheckIntegrity cfg blk =
      nodeCheckIntegrity (concreteByronNodeConfig cfg) (concreteByronBlock blk)

  -- The header is just the concrete header, so we can just reuse the Byron def
  nodeAddHeaderEnvelope = \_ -> nodeAddHeaderEnvelope pb

  -- Encoders
  nodeEncodeBlockWithInfo = const encodeDualByronBlockWithInfo
  nodeEncodeHeader        = const encodeDualByronHeader
  nodeEncodeGenTx         = encodeDualByronGenTx
  nodeEncodeGenTxId       = encodeDualByronGenTxId
  nodeEncodeHeaderHash    = const encodeDualByronHeaderHash
  nodeEncodeLedgerState   = const encodeDualByronLedgerState
  nodeEncodeChainState    = \_proxy _cfg -> encodeDualByronChainState
  nodeEncodeApplyTxError  = const encodeDualByronApplyTxError

  -- Decoders
  nodeDecodeHeader        = decodeDualByronHeader . extractEpochSlots
  nodeDecodeBlock         = decodeDualByronBlock  . extractEpochSlots
  nodeDecodeGenTx         = decodeDualByronGenTx
  nodeDecodeGenTxId       = decodeDualByronGenTxId
  nodeDecodeHeaderHash    = const decodeDualByronHeaderHash
  nodeDecodeLedgerState   = const decodeDualByronLedgerState
  nodeDecodeChainState    = \_proxy cfg ->
                               let k = pbftSecurityParam $ pbftParams cfg
                               in decodeDualByronChainState k
  nodeDecodeApplyTxError  = const decodeDualByronApplyTxError

extractEpochSlots :: NodeConfig DualByronConsensusProtocol -> EpochSlots
extractEpochSlots = Byron.extractEpochSlots . concreteByronNodeConfig
