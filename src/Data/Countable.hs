{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- SPDX-License-Identifier: BSD-3-Clause

--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Countable
-- Copyright   :  José A. Riaza
-- Maintainer  :  riaza.valverde@gmail.com
--
-- A type is a set of possible values. This module provides the 'Countable'
-- class, which computes the cardinality of any instance type.
-- >>> count :: Count Bool
-- Count {getCount = 2}
-- >>> count :: Count (Bool -> Bool -> Bool)
-- Count {getCount = 16}
-- >>> count :: Count [Void]
-- Count {getCount = 1}
--
-- Some types can have infinite values, in which case the count may not finish.
--------------------------------------------------------------------------------

module Data.Countable
    ( -- * Countable types
      Countable(..),
      Count(..),
      recount,
      countTypeOf
    ) where

import GHC.Generics( V1, U1(..), K1(K1), M1(M1), type (:+:)(..), type (:*:)(..), Generic(Rep, from) )
import Data.Void( Void )
import Data.Ratio( Ratio )

-- | Count
-- Number of elements of a type.
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

-- | recount
-- Change the count type.
recount :: Count a -> Count b
recount (Count x) = Count x

-- | countTypeOf
-- Count the number of elements of the parameter's type.
countTypeOf :: Countable a => a -> Count a
countTypeOf (_ :: a) = count :: Count a

-- | A type is a set of possible values. The 'Countable' class allows to get
--   the cardinality of a type, i.e., the number of elements in the type.
class Countable a where
    -- | 'count' is used to get the cardinality of the type.
    count :: Count a
    default count :: (Generic a, Countable' (Rep a)) => Count a
    count = recount $ count' (from (undefined :: a))

-- | Generic countable class for deriving instances.
class Countable' f where
    count' :: f a -> Count (f a)
    {-# MINIMAL count' #-}

-- | Empty type.
-- |{}| = 0
instance Countable' V1 where
    count' _ = Count 0

-- | Unit type ().
-- |{()}| = 1
instance Countable' U1 where
    count' _ = Count 1

-- | Meta-information.
-- id
instance (Countable' a) => Countable' (M1 i c a) where
    count' _ = recount $ count' (undefined :: a ())

-- | Constants.
-- id
instance (Countable a) => Countable' (K1 i a) where
    count' _ = recount (count :: Count a)

-- | Sum of types.
-- |a+b| = |a| + |b|
instance (Countable' a, Countable' b) => Countable' (a :+: b) where
    count' _ = a + b
        where a = recount (count' (undefined :: a ()))
              b = recount (count' (undefined :: b ()))

-- | Cartesian product of types.
-- |a*b| = |a| * |b|
instance (Countable' a, Countable' b) => Countable' (a :*: b) where
    count' _ = a * b
        where a = recount (count' (undefined :: a ()))
              b = recount (count' (undefined :: b ()))

-- | Numbers.
-- |Int| = 1 + maxBound - minBound
instance Countable Int where
    count = Count (1 + toInteger (maxBound :: Int) - toInteger (minBound :: Int))

-- |Integer| = Infinite
instance Countable Integer where
    count = 1 + count

-- |Word| = 1 + maxBound
instance Countable Word where
    count = Count (1 + toInteger (maxBound :: Word))

-- | Rational numbers.
-- |(Ratio a)| = |a| * |a|
instance Countable a => Countable (Ratio a) where
    count = let a = count :: Count a in recount (a*a)

-- | Characters.
-- |Char| = 1 + maxBound
instance Countable Char where
    count = Count (1 + toInteger (fromEnum (maxBound :: Char)))

-- | Functions.
-- |(a -> b)| = |b| ^ |a|
instance (Countable a, Countable b) => Countable (a -> b) where
    count = if b == 0 then Count 0 else Count (b ^ a)
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