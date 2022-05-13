{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Data.Countable
    ( -- * Countable types
      Countable(..),
      Count(..)
    ) where

import GHC.Generics( V1, U1(..), K1(K1), M1(M1), type (:+:)(..), type (:*:)(..), Generic(Rep, from, to) )
import Data.Void( Void )

data Count = Finite Integer | Infinite
    deriving (Ord, Eq, Read, Show)

instance Num Count where
    (Finite n) + (Finite m) = Finite (n+m)
    _ + _ = Infinite
    (Finite 0) * _ = Finite 0
    _ * (Finite 0) = Finite 0
    (Finite n) * (Finite m) = Finite (n*m)
    _ * _ = Infinite
    (Finite n) - (Finite m) = Finite (max 0 (n-m))
    _ - _ = Infinite
    abs x = x
    signum (Finite 0) = Finite 0
    signum _ = Finite 1
    fromInteger n | n >= 0 = Finite n
                  | otherwise = Finite 0

class Countable a where
    count :: a -> Count
    default count :: (Generic a, Countable' (Rep a)) => a -> Count
    count x = count' (from x)

-- | Generic countable class for deriving instances.
class Countable' f where
    count' :: f a -> Count
    {-# MINIMAL count' #-}

-- | Empty type.
-- 0
instance Countable' V1 where
    count' _ = Finite 0

-- | Unit type ().
-- 1
instance Countable' U1 where
    count' _ = Finite 1

-- | Meta-information.
-- n
instance (Countable' a) => Countable' (M1 i c a) where
    count' (M1 x) = count' x

-- | Constants.
-- n
instance (Countable a) => Countable' (K1 i a) where
    count' (K1 x) = count x

-- | Sum of types.
-- n+m
instance (Countable' a, Countable' b) => Countable' (a :+: b) where
    count' _ = (count' (undefined :: a ())) + (count' (undefined :: b ()))

-- | Cartesian product of types.
-- n*m
instance (Countable' a, Countable' b) => Countable' (a :*: b) where
    count' _ = (count' (undefined :: a ())) * (count' (undefined :: b ()))

instance Countable Void
instance Countable Bool
instance Countable Ordering
instance Countable a => Countable [a]
instance Countable a => Countable (Maybe a)
instance (Countable a, Countable b) => Countable (Either a b)
instance Countable ()
instance (Countable a, Countable b) => Countable (a,b)