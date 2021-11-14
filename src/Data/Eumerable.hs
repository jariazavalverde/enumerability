module Data.Enumerable(
    Enumerable(..)
) where

import Data.Maybe(mapMaybe)

-- | Enumerable class
class Enumerable a where
    encode :: a -> Int
    decode :: Int -> Maybe a
    enumerate :: [a]
    enumerate = mapMaybe decode [1..]

-- | Integers
-- 0, 1, -1, 2, -2, 3, -3, ...
instance Enumerable Int where
    encode n | n == 0 = 1
             | n > 0 = 2*n
             | otherwise = (-2)*n + 1
    decode n | n == 1 = Just 0
             | even n = Just (div n 2)
             | otherwise = Just (- div (n-1) 2)

-- | Sum
-- Left 1, Right 1, Left 2, Right 2, Left 3, Right 3, ...
instance (Enumerable a, Enumerable b) => Enumerable (Either a b) where
    encode (Left a) = encode a * 2 - 1
    encode (Right b) = encode b * 2
    decode n | even n = Right <$> decode (div n 2)
             | otherwise = Left <$> decode (div (n+1) 2)

-- | Product
-- (1,1), (2,1), (1,2), (3,1), (2,2), (1,3), ...
instance (Enumerable a, Enumerable b) => Enumerable (a,b) where
    encode (a,b) = let a' = encode a
                       b' = encode b
                       n = a' + b'
                   in b' + upto (n-2)
    decode n = let k = upto' n
                   b = n - upto k
                   a = k - b + 2
               in (,) <$> decode a <*> decode b

upto :: Int -> Int
upto n = div (n*(n+1)) 2

upto' :: Int -> Int
upto' x = (fst $ head $ filter ((>= x).snd) $ fmap (\n -> (n, upto n)) [1..]) - 1