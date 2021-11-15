{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveAnyClass #-}

module Data.Enumerable(
    Enumerable(..)
) where

import GHC.Generics( V1, U1(..), M1(M1), type (:+:)(..), type (:*:)(..), K1 (K1), Generic (Rep, from, to) )
import Data.Maybe( mapMaybe )

-- | Enumerable class
class Enumerable a where
    encode :: a -> Int
    default encode :: (Generic a, Enumerable' (Rep a)) => a -> Int
    encode a = encode' (from a)

    decode :: Int -> Maybe a
    default decode :: (Generic a, Enumerable' (Rep a)) => Int -> Maybe a
    decode n = to <$> decode' n

    enumerate :: [a]
    enumerate = mapMaybe decode [1..]

-- | Generic enumerable class
class Enumerable' f where
    encode' :: f a -> Int 
    decode' :: Int -> Maybe (f a)

-- | Integers
-- 0, 1, -1, 2, -2, 3, -3, ...
instance Enumerable Int where
    encode n | n == 0 = 1
             | n > 0 = 2*n
             | otherwise = (-2)*n + 1
    decode n | n == 1 = Just 0
             | even n = Just (div n 2)
             | otherwise = Just (- div (n-1) 2)

-- | Empty
-- 
instance Enumerable' V1 where
    encode' _ = undefined
    decode' _ = Nothing

-- | Unit
-- ()
instance Enumerable' U1 where
    encode' U1 = 1
    decode' 1 = Just U1
    decode' _ = Nothing

-- | Sum
-- Left 1, Right 1, Left 2, Right 2, Left 3, Right 3, ...
instance (Enumerable' a, Enumerable' b) => Enumerable' (a :+: b) where
    encode' (L1 a) = encode' a * 2 - 1
    encode' (R1 b) = encode' b * 2
    decode' n | even n = R1 <$> decode' (div n 2)
              | otherwise = L1 <$> decode' (div (n+1) 2)

-- | Product
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

-- | Meta-information
instance (Enumerable' a) => Enumerable' (M1 i c a) where
    encode' (M1 x) = encode' x
    decode' n = M1 <$> decode' n

-- | Constants
instance (Enumerable a) => Enumerable' (K1 i a) where
    encode' (K1 x) = encode x
    decode' n = K1 <$> decode n

-- | Derived instances
deriving instance Enumerable Bool
deriving instance Enumerable Ordering
deriving instance (Enumerable a) => Enumerable [a]
deriving instance (Enumerable a) => Enumerable (Maybe a)
deriving instance (Enumerable a, Enumerable b) => Enumerable (Either a b)
deriving instance Enumerable ()
deriving instance (Enumerable a, Enumerable b) => Enumerable (a,b)
deriving instance (Enumerable a, Enumerable b, Enumerable c) => Enumerable (a,b,c)
deriving instance (Enumerable a, Enumerable b, Enumerable c, Enumerable d) => Enumerable (a,b,c,d)
deriving instance (Enumerable a, Enumerable b, Enumerable c, Enumerable d, Enumerable e) => Enumerable (a,b,c,d,e)
deriving instance (Enumerable a, Enumerable b, Enumerable c, Enumerable d, Enumerable e, Enumerable f) => Enumerable (a,b,c,d,e,f)
deriving instance (Enumerable a, Enumerable b, Enumerable c, Enumerable d, Enumerable e, Enumerable f, Enumerable g) => Enumerable (a,b,c,d,e,f,g)

upto :: (Num n, Integral n) => n -> n
upto n = div (n*(n+1)) 2

upto' :: (Num n, Integral n) => n -> n
upto' x = head [n | n <- [1..], upto n >= x] - 1