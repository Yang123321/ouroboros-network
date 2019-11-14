{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ouroboros.Network.TxIdPSQ
  ( TxIdPSQ (..)

    -- * Query
  , member
  , intersection

    -- * Construction
  , empty

    -- * Insertion
  , insertTxId
  , insertTxIds

    -- * Expiration
  , expireTxIds
  ) where

import           Cardano.Prelude (NoUnexpectedThunks, OnlyCheckIsWHNF (..))

import           Control.Monad.Class.MonadTime (Time)
import           Data.Foldable (foldl')
import qualified Data.List as List (filter)
import qualified Data.OrdPSQ as OrdPSQ (atMostView, empty, insert, member)
import           Data.OrdPSQ (OrdPSQ)

-- | A PSQ containing the IDs of transactions which we've most recently added
-- to the mempool.
newtype TxIdPSQ txid = TxIdPSQ (OrdPSQ txid Time ())
  deriving stock (Show)
  deriving newtype (NoUnexpectedThunks)

{-----------------------------------------------------------------------------
  Query
-----------------------------------------------------------------------------}

-- | /O(log n)/. Determine whether the provided transaction ID is present in
-- the 'TxIdPSQ'.
member :: Ord txid => txid -> TxIdPSQ txid -> Bool
member txId (TxIdPSQ psq) = OrdPSQ.member txId psq

-- | /O (m*log n)/. Determine which of the provided transaction IDs already
-- exist within the provided 'TxIdPSQ'.
intersection :: Ord txid => [txid] -> TxIdPSQ txid -> [txid]
intersection txIds (TxIdPSQ psq) = List.filter (flip OrdPSQ.member psq) txIds

{-----------------------------------------------------------------------------
  Construction
-----------------------------------------------------------------------------}

-- | /O(1)/. An empty 'TxIdPSQ'.
empty :: TxIdPSQ txid
empty = TxIdPSQ (OrdPSQ.empty)

{-----------------------------------------------------------------------------
  Insertion
-----------------------------------------------------------------------------}

-- | /O(log n)/. Insert a new transaction ID and 'Time' (expected to represent
-- the time of insertion) into the 'TxIdPSQ'.
insertTxId :: Ord txid => txid -> Time -> TxIdPSQ txid -> TxIdPSQ txid
insertTxId txId time (TxIdPSQ psq) = TxIdPSQ $ OrdPSQ.insert txId time () psq

-- | /O(m*log n)/. Insert new transaction IDs and 'Time' (expected to
-- represent the time of insertion) into the 'TxIdPSQ'.
insertTxIds :: (Foldable t, Ord txid)
            => t txid
            -> Time
            -> TxIdPSQ txid
            -> TxIdPSQ txid
insertTxIds txIds time txPsq = foldl'
  (\acc txId -> insertTxId txId time acc)
  txPsq
  txIds

{-----------------------------------------------------------------------------
  Expiration
-----------------------------------------------------------------------------}

-- | Expire/remove transaction IDs that are associated with insertion 'Time's
-- less than or equal to the provided 'Time'.
--
-- The 'fst' of the result contains those transaction IDs that are expired
-- while the 'snd' of the result contains the rest of the 'TxIdPSQ' stripped
-- of those transaction IDs.
expireTxIds :: Ord txid
            => Time
            -> TxIdPSQ txid
            -> ([(txid, Time)], TxIdPSQ txid)
expireTxIds time (TxIdPSQ psq) = (expired', TxIdPSQ psq')
  where
    (expired, psq') = OrdPSQ.atMostView time psq
    expired' = [(txId, time') | (txId, time', _) <- expired]

{-----------------------------------------------------------------------------
  Orphan instances
-----------------------------------------------------------------------------}

deriving via OnlyCheckIsWHNF "OrdPSQ" (OrdPSQ k p v)
  instance NoUnexpectedThunks (OrdPSQ k p v)
