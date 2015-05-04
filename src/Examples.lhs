> {-# Language TemplateHaskell, TypeOperators #-}

> module Examples where

> import Data.List       ( genericIndex )

> import CountingFinite  ( InverseCountable ( intTo ),
>                          Countable ( asInt, asInteger ), 
>                          AllValues ( allValues ),
>                          Bot ( Bot ) )
> import FiniteTypes     ( mkFinite, genericIndex )
> import Relational


> $(mkFinite 2)
> $(mkFinite 3)
> $(mkFinite 4)
> $(mkFinite 5)
> $(mkFinite 6)
> $(mkFinite 7)
> $(mkFinite 8)
> $(mkFinite 10)
> $(mkFinite 11)
> $(mkFinite 12)
> $(mkFinite 16)
> $(mkFinite 32)
> $(mkFinite 64)
> $(mkFinite 128)
> $(mkFinite 256)
> $(mkFinite 1024)
> -- $(mkFinite 2048)
> -- $(mkFinite 4096)

> rel :: (AllValues a, AllValues b, Ord a, Ord b) => RelFunction (a * b) Bot a b
> rel = transposition pi1 .** (Constant "" pi2 ./\. (Id **. l))

> phiPartialIdentity :: (Ord a, AllValues a) => RelFunction a Bot a a
> phiPartialIdentity = Id **. l ./\. Constant "" i
 
> phiRestrict :: (Ord a, AllValues a) => Rel a a -> RelFunction a Bot a a
> phiRestrict r = phiPartialIdentity .**. (r .** phiPartialIdentity)
 
This function checks whether a set is transitive with respect to the supplied relation.
 
> phiTransitiveSets :: (Ord a, AllValues a) => Rel a a -> RelFunction a Bot Bot Bot
> phiTransitiveSets r = Complement (l .** ((s .**. s) ./\. Complement s) **. l) where 
>      s = phiRestrict r
 
> testrel :: Rel N4 N4
> testrel = fromPairs [(N4C0, [N4C1, N4C2]),
>                       (N4C1, [N4C1, N4C2, N4C3]),
>                       (N4C2, [N4C3]),
>                       (N4C3, [N4C0, N4C1])]
 
> testTournamentDim5 :: Rel N5 N5
> testTournamentDim5 = fromPairs [(N5C0, [N5C1, N5C2, N5C4]),
>                                  (N5C1, [N5C2]),
>                                  (N5C2, [N5C3, N5C4]),
>                                  (N5C3, [N5C0, N5C1, N5C4]),
>                                  (N5C4, [N5C1])]
 
> testTournamentDim6 :: Rel N6 N6
> testTournamentDim6 = fromNumbersFull
>      [(0, [1,2,4]),(1,[2,3,5]),(2,[5]),(3,[0,2,4]),(4,[1,2]),(5,[0,3,4])]
 
> testTournamentDim7 :: Rel N7 N7
> testTournamentDim7 = fromNumbersFull
>      [(0,[2,6]),(1,[0,2,4,5,6]),(2,[4,5,6]),(3,[0,1,2,5]),(4,[0,3,6]),(5,[0,4]),(6,[3,5])] 
 
> testTournamentDim10 :: Rel N10 N10
> testTournamentDim10 = fromNumbersFull [(0,[2,3,5,8]),(1,[0,3,7,8,9]),(2,[1,3,4,6,8]),(3,[5,6,8]),(4,[0,1,3,6,7,8]),(5,[1,2,4,6,8]),(6,[0,1,9]),(7,[0,2,3,5,6,9]),(8,[6,7,9]),(9,[0,2,3,4,5])]