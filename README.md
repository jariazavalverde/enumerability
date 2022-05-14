# enumerability

An enumeration is a complete, ordered listing of all the items in a collection. Enumerations are represented as functions from indices to values (a mapping from natural numbers to elements of the collection). The mapping can be partial (i.e. the enumeration may contain "holes") and elements can be repeated, as long as they all appear at least once. A type is a set of possible values. This package provides classes for enumerate and count almost any datatype.

## Data.Enumerable

This module provides the `Enumerable` class, which computes the enumeration of any instance type.

```haskell
ghci> take 20 $ enumerate :: [Int]
[0,1,-1,2,-2,3,-3,4,-4,5,-5,6,-6,7,-7,8,-8,9,-9,10]

ghci> take 10 $ enumerate :: [(Int, Bool)]
[(0,False),(1,False),(0,True),(-1,False),(1,True),(2,False),(-1,True),(-2,False),(2,True),(3,False)]

ghci> encode not
6

ghci> (decode 6 :: Maybe (Bool -> Bool)) <*> Just True
Just False
```

Instances for generic types can be automatically derived.

## Data.Countable

This module provides the `Countable` class, which computes the cardinality of any instance type.

```haskell
ghci> count :: Count Bool
Count {getCount = 2}

ghci> count :: Count (Bool -> Bool -> Bool)
Count {getCount = 16}

ghci> count :: Count [Void]
Count {getCount = 1}
```

Instances for generic types can be automatically derived.