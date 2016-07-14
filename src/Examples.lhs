> {-# Language TypeOperators #-}

> module Examples where

> import Data.Typeable   ( Typeable )
>
> import CountingFinite ( AllValues, Bot )
> import FiniteTypes     
> import Relational

An exemplary empty relation called "A".

> ra :: Rel N4 N5
> ra = fromPairs [] (Plain "A")

An exemplary empty relation called "B".

> rb :: Rel N4 N5
> rb = fromPairs [] (Plain "B")

An exemplary empty relation called "C".

> rc :: Rel N4 N5
> rc = fromPairs [] (Plain "C")

The function "rel", which transforms an (a * b, Bot)-vector into an (a * b)-relation.

> rel :: (Typeable a, Typeable b, AllValues a, AllValues b, Ord a, Ord b) => 
>     RelFunction (a * b) Bot a b
> rel = transposition pi1 .** (Constant pi2 ./\. (Id **. l))

The function that maps a vector to its corresponding partial identity

> phiPartialIdentity :: (Typeable a, Ord a, AllValues a) => RelFunction a Bot a a
> phiPartialIdentity = Id **. l ./\. Constant i
 
The function that maps a relation to its restriction (domain and codomain) to a vector.

> phiRestrict :: (Typeable a, Ord a, AllValues a) => Rel a a -> RelFunction a Bot a a
> phiRestrict r = phiPartialIdentity .**. (r .** phiPartialIdentity)

Maps a set (represented by a vector) to the Cartesian product of the set with itself.

> phiSetPairs :: (Ord a, AllValues a) => RelFunction a Bot a a
> phiSetPairs = Id .**. Transposition Id
 
This function checks whether a set is transitive with respect to the supplied relation.
 
> phiTransitiveSets :: (Typeable a, Ord a, AllValues a) => Rel a a -> RelFunction a Bot Bot Bot
> phiTransitiveSets r = Complement (l .** ((s .**. s) ./\. Complement s) **. l) where 
>      s = phiRestrict r

> phiTransitiveSets2 :: (Typeable a, Ord a, AllValues a) => Rel a a -> RelFunction a Bot Bot Bot
> phiTransitiveSets2 r = Complement (l .** ((s .**. s) ./\. Complement s) **. l) where
>   s = Constant r ./\. phiSetPairs

A non-empty test relation.

> plainR :: Rel A A
> plainR = fromNumbersFull [] (Plain "R")

> testrel :: Rel N4 N4
> testrel = fromNumbersFull [(0, [1, 2]),
>                       (1, [1, 2, 3]),
>                       (2, [3]),
>                       (3, [0, 1])] (Plain "R")

A non-empty tournament with five players.

> testTournamentDim5 :: Rel N5 N5
> testTournamentDim5 = fromNumbersFull [(0, [1, 2, 4]),
>                                  (1, [2]),
>                                  (2, [3, 4]),
>                                  (3, [0, 1, 4]),
>                                  (4, [1])] (Plain "T5")
 
A non-empty tournament with six players.

> testTournamentDim6 :: Rel N6 N6
> testTournamentDim6 = fromNumbersFull
>      [(0, [1,2,4]),
>       (1,[2,3,5]),
>       (2,[5]),
>       (3,[0,2,4]),
>       (4,[1,2]),
>       (5,[0,3,4])]
>      (Plain "T6")

A non-empty tournament with seven players. 

> testTournamentDim7 :: Rel N7 N7
> testTournamentDim7 = fromNumbersFull
>      [(0,[2,6]),
>       (1,[0,2,4,5,6]),
>       (2,[4,5,6]),
>       (3,[0,1,2,5]),
>       (4,[0,3,6]),
>       (5,[0,4]),
>       (6,[3,5])] 
>      (Plain "T7")
 
A non-empty tournament with ten players.

> testTournamentDim10 :: Rel N10 N10
> testTournamentDim10 = fromNumbersFull 
>   [(0,[2,3,5,8]),
>    (1,[0,3,7,8,9]),
>    (2,[1,3,4,6,8]),
>    (3,[5,6,8]),
>    (4,[0,1,3,6,7,8]),
>    (5,[1,2,4,6,8]),
>    (6,[0,1,9]),
>    (7,[0,2,3,5,6,9]),
>    (8,[6,7,9]),
>    (9,[0,2,3,4,5])] 
>   (Plain "T10")