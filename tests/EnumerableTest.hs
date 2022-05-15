{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Test.QuickCheck( quickCheck, Property, (==>) )
import Data.Functor.Const( Const(..) )
import Data.Maybe( isJust )
import Data.Enumerable

-- | prop_encode_decode_id
-- Decode an encoded value should return the same value.
-- 'decode . encode == Just'
prop_encode_decode_id :: (Enumerable a, Eq a) => a -> Bool
prop_encode_decode_id x =
    decode (encode x) == Just x

-- | prop_decode_encode_id
-- Encode a (valid) decoded value should return the same code.
-- 'fmap encode . decode == Just'
prop_decode_encode_id :: Enumerable a => Const Integer a -> Property
prop_decode_encode_id (n :: Const Integer a) =
    n' > 0 && isJust x ==> fmap encode x == Just n'
        where n' = getConst n
              x = decode n' :: Maybe a

main = do quickCheck (prop_encode_decode_id :: Maybe Char -> Bool)
          quickCheck (prop_encode_decode_id :: (Int,Bool) -> Bool)
          quickCheck (prop_encode_decode_id :: Either Int Bool -> Bool)
          quickCheck (prop_decode_encode_id :: Const Integer (Maybe Char) -> Property)
          quickCheck (prop_decode_encode_id :: Const Integer (Int,Int) -> Property)
          quickCheck (prop_decode_encode_id :: Const Integer (Either Int Int) -> Property)