-- | Assorted utility functions for TPraos integration.
--
-- In particular, various things we need for integration with the `delegation`
-- package from cardano-ledger-specs.
module Ouroboros.Consensus.Protocol.TPraos.Util where

import Ouroboros.Network.Block (SlotNo (..))
import Ouroboros.Network.Point (WithOrigin(..))

-- | Verify whether a slot represents a change to a new epoch with regard to
-- some other slot.
isNewEpoch :: SlotNo -> WithOrigin SlotNo -> Bool
isNewEpoch = undefined
