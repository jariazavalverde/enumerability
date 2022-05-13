{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Data.Countable
    ( -- * Countable types
      Countable(..),
      Count(..)
    ) where

import GHC.Generics( V1, U1(..), K1(K1), M1(M1), type (:+:)(..), type (:*:)(..), Generic(Rep, from) )
import Data.Void( Void )

newtype Count a = Count { getCount :: Integer }
    deriving (Ord, Eq, Read, Show)

instance Num (Count a) where
    (Count 0) + m = m
    (Count n) + (Count m) = Count (n+m)
    (Count 0) * _ = Count 0
    (Count 1) * m = m
    (Count n) * (Count m) = Count (n*m)
    (Count n) - (Count m) = Count (max 0 (n-m))
    abs (Count x) = Count (abs x)
    signum (Count n) = Count (signum n)
    fromInteger n | n >= 0 = Count n
                  | otherwise = Count 0

recount :: Count a -> Count b
recount (Count x) = Count x

class Countable a where
    count :: Count a
    default count :: (Generic a, Countable' (Rep a)) => Count a
    count = recount $ count' (from (undefined :: a))

-- | Generic countable class for deriving instances.
class Countable' f where
    count' :: f a -> Count (f a)
    {-# MINIMAL count' #-}

-- | Empty type.
-- 0
instance Countable' V1 where
    count' _ = Count 0

-- | Unit type ().
-- 1
instance Countable' U1 where
    count' _ = Count 1

-- | Meta-information.
-- n
instance (Countable' a) => Countable' (M1 i c a) where
    count' _ = recount $ count' (undefined :: a ())

-- | Constants.
-- n
instance (Countable a) => Countable' (K1 i a) where
    count' _ = recount (count :: Count a)

-- | Sum of types.
-- n+m
instance (Countable' a, Countable' b) => Countable' (a :+: b) where
    count' _ = a + b
        where a = recount (count' (undefined :: a ()))
              b = recount (count' (undefined :: b ()))

-- | Cartesian product of types.
-- n*m
instance (Countable' a, Countable' b) => Countable' (a :*: b) where
    count' _ = a * b
        where a = recount (count' (undefined :: a ()))
              b = recount (count' (undefined :: b ()))

-- | Functions.
instance (Countable a, Countable b) => Countable (a -> b) where
    count = Count (b ^ a)
        where a = getCount (count :: Count a)
              b = getCount (count :: Count b)

-- | Automatic derived instances.
instance Countable Void
instance Countable Bool
instance Countable Ordering
instance Countable a => Countable [a]
instance Countable a => Countable (Maybe a)
instance (Countable a, Countable b) => Countable (Either a b)
instance Countable ()
instance (Countable a, Countable b) => Countable (a,b)
instance (Countable a, Countable b, Countable c) => Countable (a,b,c)
instance (Countable a, Countable b, Countable c, Countable d) => Countable (a,b,c,d)
instance (Countable a, Countable b, Countable c, Countable d, Countable e) => Countable (a,b,c,d,e)
instance (Countable a, Countable b, Countable c, Countable d, Countable e, Countable f) => Countable (a,b,c,d,e,f)
instance (Countable a, Countable b, Countable c, Countable d, Countable e, Countable f, Countable g) => Countable (a,b,c,d,e,f,g)