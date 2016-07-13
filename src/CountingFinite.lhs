> {-# Language ScopedTypeVariables, DeriveDataTypeable #-}

> module CountingFinite where

> import Data.Typeable ( Typeable )

The data type consisting of precisely one defined value.

> data Bot = Bot deriving ( Eq, Ord, Typeable )

> instance Show Bot where

>     show Bot = "_"

A type class for (finite) types that can be collected in a list.

> class AllValues a where

>    allValues :: [a]

> instance (AllValues a, AllValues b) => AllValues (a, b) where

>    allValues = [(a, b) | a <- allValues, b <- allValues]

> instance AllValues Bot where

>    allValues = [Bot]

Class for types that can be counted (countable types).
The function `asInteger` should be injective.

> class Countable i where

>    asInteger :: i -> Integer
>
>    asInt :: i -> Int
>    asInt = fromInteger . asInteger

> instance Countable Int where
>
>    asInt     = id
>    asInteger = fromIntegral

This instance is valid and minimal in the sense that the function  
`f : [0 .. a] -> [0 .. b] -> [0 .. (a+1)*(b+1) - 1], (x, y) -> (1 + b) * x + y`
is bijective.

> instance (Countable a, Countable b, Bounded a, Bounded b) => Countable (a, b) where
>
>    asInteger (a, b) = (1 + asInteger (maxBound :: b)) * asInteger a + asInteger b

An inversion of the `Countable` class.
Provides a function that transforms integers into objects of the type.

> class InverseCountable i where
>
>    intTo :: Integer -> i