> {-# Language ScopedTypeVariables, DeriveDataTypeable #-}

> module CountingFinite where

> import Control.Arrow ( (***) )
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

When restricted to the natural numbers,
this function is the inverse of the Cantor function \(a, b) -> b + (a + b) * (a + b + 1) / 2.
Note that its definition is only sensible on the natural numbers,
since it is not injective on the rest.

> countProduct :: Integer -> (Integer, Integer)
> countProduct n | n <= 0 = (0, 0)
>                | otherwise = let (a, b) = countProduct (n - 1)
>                              in if a == 0 then (b + 1, 0) else (a - 1, b + 1)

The following is an injective function, whose image are the natural numbers.
It is thus a bijection from Z to N.

> countIntegers :: Integer -> Integer
> countIntegers n | n >= 0    = 2 * n
>                 | otherwise = - (2 * n + 1)

This function is the inverse of countIntegers, when the latter is restricted to the natural numbers.

> uncountIntegers :: Integer -> Integer
> uncountIntegers n | even n    = n `div` 2
>                   | otherwise = -((1 + n) `div` 2)

> instance (InverseCountable a, InverseCountable b) => InverseCountable (a, b) where
>   intTo i = (intTo x, intTo y) where
>     (x, y) = (uncountIntegers *** uncountIntegers) (countProduct (countIntegers i))