{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}

module Peano
    ( -- * Natural numbers
      Nat(..)
    ) where

import GHC.Generics( Generic )
import Data.Enumerable( Enumerable )

-- | Natural numbers (Peano representation).
-- Zero, Succ Zero, Succ (Succ Zero), ...
data Nat = Zero | Succ Nat
    deriving (Generic, Show, Eq)
    deriving anyclass (Enumerable)