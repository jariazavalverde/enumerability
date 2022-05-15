{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Test.QuickCheck( quickCheck )
import Data.Void( Void )
import Data.Functor.Const( Const(..) )
import Data.Countable

-- | prop_countable_empty
-- The cardinality of an empty type should be 0.
-- '|void| = 0'
prop_countable_empty :: Bool
prop_countable_empty = (count :: Count Void) == 0

-- | prop_countable_unit
-- The cardinality of a unit type should be 1.
-- '|unit| = 1'
prop_countable_unit :: Bool
prop_countable_unit = (count :: Count ()) == 1

-- | prop_countable_sum
-- The cardinality of a sum type should be the sum of the cardinalities.
-- '|n+m| = |n| + |m|'
prop_countable_sum :: (Countable n, Countable m) => Const () (n,m) -> Bool
prop_countable_sum (Const _ :: Const () (n,m)) = sum == recount n + recount m
    where n = count :: Count n
          m = count :: Count m
          sum = count :: Count (Either n m)

-- | prop_countable_product
-- The cardinality of a product type should be the product of the cardinalities.
-- '|n*m| = |n| * |m|'
prop_countable_product :: (Countable n, Countable m) => Const () (n,m) -> Bool
prop_countable_product (Const _ :: Const () (n,m)) = prod == recount n * recount m
    where n = count :: Count n
          m = count :: Count m
          prod = count :: Count (n,m)

main = do quickCheck prop_countable_empty
          quickCheck prop_countable_unit
          quickCheck (prop_countable_sum :: Const () (Char,Bool) -> Bool)
          quickCheck (prop_countable_sum :: Const () (Int,Ordering) -> Bool)
          quickCheck (prop_countable_product :: Const () (Char,Bool) -> Bool)
          quickCheck (prop_countable_product :: Const () (Int,Ordering) -> Bool)