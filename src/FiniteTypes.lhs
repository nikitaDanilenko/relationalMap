> {-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}

This module provides a large number of convenient finite types.

> module FiniteTypes where
>
> import Data.List      ( genericIndex )
> import Data.Typeable  ( Typeable )
>
> import CountingFinite ( InverseCountable ( intTo ),
>                         Countable ( asInt, asInteger ), 
>                         AllValues ( allValues ),
>                         Bot ( Bot ) )
> import MkFiniteTypes  ( mkFinite, mkAlphabetic )

Several finite types with generic names and constructors.
The type NK has K inhabitants and is constructors are labelled NKC0 until NKC{K-1}.

> $(mkFinite 1)
> $(mkFinite 2)
> $(mkFinite 3)
> $(mkFinite 4)
> $(mkFinite 5)
> $(mkFinite 6)
> $(mkFinite 7)
> $(mkFinite 8)
> $(mkFinite 9)
> $(mkFinite 10)
> $(mkFinite 11)
> $(mkFinite 12)
> $(mkFinite 13)
> $(mkFinite 14)
> $(mkFinite 15)
> $(mkFinite 16)
> $(mkFinite 17)
> $(mkFinite 18)
> $(mkFinite 19)
> $(mkFinite 20)
> $(mkFinite 32)
> $(mkFinite 64)
> $(mkFinite 128)
> $(mkFinite 256)
> $(mkFinite 1024)

Finite one-letter types. These are useful for representing relational settings without
explicitly resorting to numbered types.
Whenever you think of a polymorphic function or relation with one letter type variables,
you can simply instantiate these variables with the corresponding upper case letters to obtain
a monomorphic instance for further computations.

> $(mkAlphabetic 'a')
> $(mkAlphabetic 'b')
> $(mkAlphabetic 'c')
> $(mkAlphabetic 'd')
> $(mkAlphabetic 'e')
> $(mkAlphabetic 'f')
> $(mkAlphabetic 'g')
> $(mkAlphabetic 'h')
> $(mkAlphabetic 'i')
> $(mkAlphabetic 'j')
> $(mkAlphabetic 'k')
> $(mkAlphabetic 'l')
> $(mkAlphabetic 'm')
> $(mkAlphabetic 'n')
> $(mkAlphabetic 'o')
> $(mkAlphabetic 'p')
> $(mkAlphabetic 'q')
> $(mkAlphabetic 'r')
> $(mkAlphabetic 's')
> $(mkAlphabetic 't')
> $(mkAlphabetic 'u')
> $(mkAlphabetic 'v')
> $(mkAlphabetic 'w')
> $(mkAlphabetic 'x')
> $(mkAlphabetic 'y')
> $(mkAlphabetic 'z')