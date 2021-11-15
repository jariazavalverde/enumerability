{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}

-- SPDX-License-Identifier: BSD-3-Clause

--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Enumerable
-- Copyright   :  José A. Riaza
-- Maintainer  :  riaza.valverde@gmail.com
--
-- An enumeration is a complete, ordered listing of all the items in a 
-- collection. Enumerations are represented as functions from indices to values
-- (a mapping from natural numbers to elements of the collection).
-- >>> take 20 $ enumerate :: [Int]
-- [0,1,-1,2,-2,3,-3,4,-4,5,-5,6,-6,7,-7,8,-8,9,-9,10]
-- >>> take 10 $ enumerate :: [(Int, Bool)]
-- [(0,False),(1,False),(0,True),(-1,False),(1,True),(2,False),(-1,True),(-2,False),(2,True),(3,False)]
--
-- The mapping can be partial (i.e. the enumeration may contain "holes") and
-- elements can be repeated, as long as they all appear at least once.
--------------------------------------------------------------------------------

module Data.Enumerable
    ( -- * Enumerable types
      Enumerable(..)
    ) where

import GHC.Generics( V1, U1(..), K1(K1), M1(M1), type (:+:)(..), type (:*:)(..), Generic(Rep, from, to) )
import Data.Maybe( mapMaybe )
import Data.Char( chr, ord )

--------------------------------------------------------------------------------
-- Enumerable types
--------------------------------------------------------------------------------

-- | An enumeration of a finite or countably infinite set of values.
--   An enumeration is represented as a pair of functions: a mapping from values 
--   to naturals (encode) and a (possible partial) mapping from naturals to 
--   values (decode).
class Enumerable a where
    -- | 'encode' is used to get the index of an element of the enumeration.
    encode :: a -> Integer
    default encode :: (Generic a, Enumerable' (Rep a)) => a -> Integer
    encode a = encode' (from a)
    -- | 'decode' is used to get an element of the enumeration from its index.
    decode :: Integer -> Maybe a
    default decode :: (Generic a, Enumerable' (Rep a)) => Integer -> Maybe a
    decode n = to <$> decode' n
    -- | 'enumerate' lists all non-empty elements of the enumeration.
    enumerate :: [a]
    enumerate = mapMaybe decode [1..]

-- | Generic enumerable class for deriving instances.
class Enumerable' f where
    encode' :: f a -> Integer
    decode' :: Integer -> Maybe (f a)
    {-# MINIMAL encode', decode' #-}

-- | Empty type.
instance Enumerable' V1 where
    encode' _ = undefined
    decode' _ = Nothing

-- | Unit type ().
instance Enumerable' U1 where
    encode' U1 = 1
    decode' 1 = Just U1
    decode' _ = Nothing

-- | Meta-information.
instance (Enumerable' a) => Enumerable' (M1 i c a) where
    encode' (M1 x) = encode' x
    decode' n = M1 <$> decode' n

-- | Constants.
instance (Enumerable a) => Enumerable' (K1 i a) where
    encode' (K1 x) = encode x
    decode' n = K1 <$> decode n

-- | Sum of types.
-- Left 1, Right 1, Left 2, Right 2, Left 3, Right 3, ...
instance (Enumerable' a, Enumerable' b) => Enumerable' (a :+: b) where
    encode' (L1 a) = encode' a * 2 - 1
    encode' (R1 b) = encode' b * 2
    decode' n | even n = R1 <$> decode' (div n 2)
              | otherwise = L1 <$> decode' (div (n+1) 2)

-- | Cartesian product of types.
-- (1,1), (2,1), (1,2), (3,1), (2,2), (1,3), ...
instance (Enumerable' a, Enumerable' b) => Enumerable' (a :*: b) where
    encode' (a :*: b) = let a' = encode' a
                            b' = encode' b
                            n = a' + b'
                        in b' + upto (n-2)
    decode' n = let k = upto' n
                    b = n - upto k
                    a = k - b + 2
                in (:*:) <$> decode' a <*> decode' b

upto :: (Num n, Integral n) => n -> n
upto n = div (n*(n+1)) 2

-- upto' x = head [n | n <- [1..], upto n >= x] - 1
upto' :: (Num n, Integral n) => n -> n
upto' x = ceiling ((-1 + sqrt (1 + 8 * fromIntegral x)) / 2) - 1

-- | Integers.
-- 0, 1, -1, 2, -2, 3, -3, ...
instance Enumerable Int where
    encode = encodeNum
    decode = decodeNum

instance Enumerable Integer where
    encode = encodeNum
    decode = decodeNum

encodeNum :: (Num a, Integral a) => a -> Integer
encodeNum n | n == 0 = 1
            | n > 0 = 2 * fromIntegral n
            | otherwise = (-2) * fromIntegral n + 1

decodeNum :: (Num a, Integral a) => Integer -> Maybe a
decodeNum n | n == 1 = Just 0
            | even n = Just (div (fromInteger n) 2)
            | otherwise = Just (- div (fromInteger n-1) 2)

-- | Non-negative numbers.
-- 0, 1, 2, 3, 4, ...
instance Enumerable Word where
    encode = fromIntegral . (1+)
    decode n | n > 0 = Just $ fromIntegral n - 1
             | otherwise = Nothing

-- | Real numbers.


-- | Characters.
-- \NUL, \SOH, \STX, \ETX, \EOT, ...
instance Enumerable Char where
    encode = fromIntegral . (1+) . ord
    decode n | n > 0 = Just (chr $ fromIntegral n - 1)
             | otherwise = Nothing

-- | Automatic derived instances.
instance Enumerable Bool
instance Enumerable Ordering
instance (Enumerable a) => Enumerable [a]
instance (Enumerable a) => Enumerable (Maybe a)
instance (Enumerable a, Enumerable b) => Enumerable (Either a b)
instance Enumerable ()
instance (Enumerable a, Enumerable b) => Enumerable (a,b)
instance (Enumerable a, Enumerable b, Enumerable c) => Enumerable (a,b,c)
instance (Enumerable a, Enumerable b, Enumerable c, Enumerable d) => Enumerable (a,b,c,d)
instance (Enumerable a, Enumerable b, Enumerable c, Enumerable d, Enumerable e) => Enumerable (a,b,c,d,e)
instance (Enumerable a, Enumerable b, Enumerable c, Enumerable d, Enumerable e, Enumerable f) => Enumerable (a,b,c,d,e,f)
instance (Enumerable a, Enumerable b, Enumerable c, Enumerable d, Enumerable e, Enumerable f, Enumerable g) => Enumerable (a,b,c,d,e,f,g)