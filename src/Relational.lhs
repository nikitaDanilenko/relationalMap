> {-# Language GADTs, ScopedTypeVariables, TypeOperators #-}

> module Relational where

> import Control.Arrow           ( second, (&&&), (***) )
> import Data.Bits               ( testBit )
> import Data.List               ( intercalate, elemIndex, transpose, genericIndex )
> import Data.Map                ( Map, union, unionWith, intersectionWith, intersection, (\\), empty,
>                                  mapKeys, keys, elems, difference, differenceWith )
> import qualified Data.Map as M ( lookup, fromList, toList )
> import Data.Maybe              ( fromJust, isJust, fromMaybe )

> import CountingFinite          ( InverseCountable ( intTo ),
>                                  Countable ( asInt, asInteger ), 
>                                  AllValues ( allValues ),
>                                  Bot ( Bot ) )

Composition of functions with different arities: g <.> f :: x -> y -> z; (g <.> f) x y = g (f x y)

> (<.>) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
> (<.>) = (.) . (.)

Operations on the data type of relations
========================================

Checks whether an uncurries pair is contained in a relation.

> at :: (Ord a, Ord b) => Rel a b -> a -> b -> Bool
> at (Rel rm _) x y = isJust (M.lookup x rm >>= M.lookup y)

Data type for relations, represented in the adjacency list format.

> data Rel a b = Rel { related :: Map a (Map b ()), symbolicName :: String }

Returns the domain of a relation.

> dom :: Rel a b -> [a]
> dom = keys . related

Returns the codomain of a relation.

> cod :: Rel a b -> [b]
> cod = concat . elems . fmap keys . related

Creates a relation from a list of adjacency lists.

> fromPairs :: (Ord a, Ord b) => [(a, [b])] -> String -> Rel a b
> fromPairs ps = Rel (M.fromList ( map (\(a, bs) -> (a, M.fromList (map (\b -> (b, ())) bs))) ps ))

The function `fromNumbers` takes a "relation" of indices and transforms the indices into actual 
values.
 
> fromNumbers :: (InverseCountable a, InverseCountable b) => [(Integer, [Integer])] -> [(a, [b])]
> fromNumbers = map (intTo *** map intTo)

Creates a relation from a "relation" of indices.
  
> fromNumbersFull :: (InverseCountable a, InverseCountable b, Ord a, Ord b) 
>                 => [(Integer, [Integer])] -> String -> Rel a b
> fromNumbersFull = fromPairs . fromNumbers

Creates a special case of a relational point.
In this case this is a relation that contains exactly one arrow.

> point :: (Ord a, Ord b, Show a, Show b) => a -> b -> Rel a b
> point x y = fromPairs [(x, [y])] (show (x, y))

Pretty-prints a relation as a Boolean matrix.

> instance (Show a, Show b, AllValues a, AllValues b, Ord a, Ord b) => Show (Rel a b) where
> 
>     show rf =    prettify bvs
>                     ++ intercalate "\n" [ unwords (show a : map (toBinary a) bvs) | a <- avs ]
>         where toBinary a b | at rf a b = "X"
>                            | otherwise = "-"
>               avs = allValues
>               bvs = allValues

Auxiliary functions for Show instance.
--------------------------------------

These are used to write the domain and codomain values in a fashion that looks good independent
of the length of the names of the actual values.

> prettify :: Show a => [a] -> String
> prettify = unlines . map (("  " ++) . unwords . map return) . transpose . homShow

> homShow :: Show a => [a] -> [String]
> homShow xs = map (\(l, z) -> replicate (m - l) ' ' ++ z) zs where
>     ys = map show xs
>     zs = map (length &&& id) ys
>     m  = maximum (map fst zs)

Relational constructs
=====================

Prettier product type.

> type a * b = (a, b)

Constants
---------

The universal relation.

> l :: (Ord a, Ord b, AllValues a, AllValues b) => Rel a b
> l = fromPairs [(x, allValues)| x <- allValues] largestText

The empty relation.

> o :: Rel a b
> o = Rel empty emptyText

The identical relation.

> i :: (Ord a, AllValues a) => Rel a a
> i = fromPairs [(x, [x]) | x <- allValues] identityText

The first projection relation. Essentially, this is fst as a relation

> pi1 :: (Ord a, Ord b, AllValues a, AllValues b) => Rel (a * b) a
> pi1 = fromPairs [((x, y), [x]) | x <- allValues, y <- allValues] pi1Text

The second projection relation. Essentially, this is snd as a relation.

> pi2 :: (Ord a, Ord b, AllValues a, AllValues b) => Rel (a * b) b
> pi2 = fromPairs [((x, y), [y]) | x <- allValues, y <- allValues] pi2Text

Returns a powerset-like relation. The "quasi" is simply because p does not need to be a
power type, but can be any type.

> quasipower :: (AllValues a, AllValues p, Countable a, Countable p, Ord a, Ord p) => Rel a p
> quasipower = 
>   fromPairs [(x, [set | set <- allValues, asInteger set `testBit` asInt x]) | x <- allValues] 
>             quasipowerText


Relational operations
---------------------

The union of two relations.

> infixr 5 \/
> (\/) :: (Ord a, Ord b) => Rel a b -> Rel a b -> Rel a b
> Rel rm rn \/ Rel sm sn = Rel (unionWith union rm sm) (unwords [rn, unionSymbol, sn])

The union of a list of a relation, which is a folded binary union.

> bigunion :: (Ord a, Ord b) => [Rel a b] -> Rel a b
> bigunion = foldr (\/) o

The intersection of two relations.

> infixr 6 /\
> (/\) :: (Ord a, Ord b) => Rel a b -> Rel a b -> Rel a b
> Rel rm rn /\ Rel sm sn = Rel (intersectionWith intersection rm sm) 
>                              (unwords [rn, intersectionSymbol, sn])

The complement of a relation.

> complement :: (Ord a, Ord b, AllValues a, AllValues b) => Rel a b -> Rel a b
> complement (Rel rm rn) = Rel (differenceWith (Just <.> difference) (related l) rm)
>                              (parenPrepend complementText rn)

The transposition of a relation.

> transposition :: (Ord a, Ord b) => Rel a b -> Rel b a
> transposition rel = fromPairs [(y, [ x | x <- d, at rel x y ]) | y <- cod rel]
>                     (parenPrepend transpositionText (symbolicName rel))
>    where d = dom rel

The composition of two relations.

> infixr 7 .*.
> (.*.) :: (Ord a, Ord b, Ord c, AllValues b) => Rel a b -> Rel b c -> Rel a c
> f .*. g = fromPairs [ (a , [c | c <- codG, b <- allValues, at f a b && at g b c]) | a <- dom f]
>                     (unwords [symbolicName f, compositionSymbol, symbolicName g])
>     where codG = cod g

Parallel composition of relations.
This is the relational version of the function `(***)` from `Control.Arrow`,
where `f *** g = \(x, y) -> (f x, g y)`.

> (.||.) :: (Ord a, Ord b, Ord c, Ord d) => Rel a b -> Rel c d -> Rel (a * c) (b * d)
> f .||. g = fromPairs [((a, c), [(b, d) | b <- cf, d <- cg, at f a b && at g c d]) | a <- dom f, c <- dom g]
>                      (unwords [symbolicName f, parallelSymbol, symbolicName g])
>    where cf = cod f
>          cg = cod g

Auxiliary relational functions
------------------------------

This function identifies the types `b` and `b * Bot`.
It is the inverse of `isomorphic'`.

> isomorphic :: (Ord a, Ord b) => Rel a b -> Rel a (b * Bot)
> isomorphic (Rel rm rn) = Rel (fmap (mapKeys f) rm) rn where
>    f b =  (b, Bot)

This function identifies the types `b * Bot` and `b`.
It is the inverse of `isomorphic`.

> isomorphic' :: (Ord a, Ord b) => Rel a (Bot * b) -> Rel a b
> isomorphic' (Rel rm rn) = Rel (fmap (mapKeys snd) rm) rn

This function creates an injection in the q-th component.
The supplied relation should be a point for proper behaviour.
In theory, we have `iotaQ q = transposition (pi2 /\ pi1 .*. q .*. l)`.
However, since this expression contains several relational operations and two
large relations pi1 and pi2,
we express it in concrete terms.

> iotaQ :: (AllValues a, AllValues b, Ord a, Ord b) => Rel b Bot -> Rel a (b * a)
> iotaQ q = fromPairs [(a, [(m, a) | m <- allValues, at q m Bot]) | a <- allValues]
>                     (concat [injectionText, "-", symbolicName q])

The "theoretical" version of `iotaQ`

> iotaQPure :: (AllValues a, AllValues b, Ord a, Ord b) => Rel b Bot -> Rel a (b * a)
> iotaQPure q = transposition (pi2 /\ pi1 .*. q .*. l)

Injections can be used to express the sum of several relations by placing
relations at different blocks.
The `bigsum` function takes a sum generator and applies it to all points of a type.

> bigsum :: (Ord a, Ord b, Ord m, AllValues b, AllValues m, Show m) => (m -> Rel a b) -> Rel a (m * b)
> bigsum r = bigunion (map (\q -> (r q) .*. iotaQ (point q Bot)) allValues)

Functions that map relations to relations
=========================================

At the heart of the implementation we have relational functions, which are concrete
functions that take a relation and yield a relation.
Since we wish to manipulate these functions in an algebraic fashion,
we define a (generalised algebraic) data type for these functions to represent
several "good" possibilities and the general case.
We use GADTs to express the dependency on arbitrary intermediate types in a proper fashion.

We begin with a simple data type that represents the heterogeneous binary operations.

> data BinaryOp = Union | Intersection

This function transforms the abstract operation into an actual one.

> toFunction :: (Ord a, Ord b)  => BinaryOp -> Rel a b -> Rel a b -> Rel a b
> toFunction Union        = (\/)
> toFunction Intersection = (/\)

This function assigns a precedence to the abstract operations.

> precOf :: BinaryOp -> Int
> precOf Union        = 4
> precOf Intersection = 5

The constructor `Constant` denotes a constant function, where the `String` value is its
symbolic name and `Id` is the identical function.
The constructors `WithBinary op` and `Prod` denote the pointwise union, intersection
and product of relational functions respectively.
The constructors `LProd` and `RProd` denote the left and right multiplication with a
constant and are special cases of the `Prod` constructor.
The `Complement` and `Transposition` constructors are the pointwise versions of
the respective relational operations.
Finally, the `None` constructor denotes the general case,
where none of the above cases matches


> data RelFunction a b c d where
>
>    Constant      :: String -> Rel c d -> RelFunction a b c d
>    Id            :: RelFunction a b a b
>    WithBinary    :: BinaryOp -> RelFunction a b c d -> RelFunction a b c d -> RelFunction a b c d
>    Complement    :: RelFunction a b c d -> RelFunction a b c d
>    LProd         :: (AllValues c, Ord c) => Rel e c -> RelFunction a b c d -> RelFunction a b e d
>    RProd         :: (AllValues d, Ord d) => RelFunction a b c d -> Rel d e -> RelFunction a b c e
>    Prod          :: (AllValues d, Ord d) => RelFunction a b c d -> RelFunction a b d e
>                                                                 -> RelFunction a b c e
>    Transposition :: RelFunction a b c d -> RelFunction a b d c
>    None          :: (Rel a b -> Rel c d) -> RelFunction a b c d

Infix variants of the costructors.

> infixr 4 .\/.
> (.\/.) :: RelFunction a b c d -> RelFunction a b c d -> RelFunction a b c d
> (.\/.) = WithBinary Union

> infixr 5 ./\.
> (./\.) :: RelFunction a b c d -> RelFunction a b c d -> RelFunction a b c d
> (./\.) = WithBinary Intersection

> infixr 6 .**
> (.**) :: (AllValues c, Ord c) => Rel e c -> RelFunction a b c d -> RelFunction a b e d
> (.**) = LProd

> infixr 6 **.
> (**.) :: (AllValues d, Ord d) => RelFunction a b c d -> Rel d e -> RelFunction a b c e
> (**.) = RProd

> infixr 7 .**.
> (.**.) :: (AllValues d, Ord d) => RelFunction a b c d -> RelFunction a b d e -> RelFunction a b c e
> (.**.) = Prod

Abstract relational functions can be applied to relations.
The apply function transforms the abstract function into a concrete one.

> apply :: (Ord c, Ord d, AllValues c, AllValues d)  => RelFunction a b c d -> Rel a b -> Rel c d
> apply (Constant  _ c)     _ = c
> apply Id                  r = r
> apply (WithBinary op f g) r = toFunction op (apply f r) (apply g r)
> apply (Complement f)      r = complement (apply f r)
> apply (LProd x f)         r = x .*. apply f r
> apply (RProd f y)         r = apply f r .*. y
> apply (Prod f g)          r = apply f r .*. apply g r
> apply (Transposition f)   r = transposition (apply f r)
> apply (None f)            r = f r

The relational map function takes a relational function denoted by an argument
of the type `RelFunction` and yields a relational function.
An actual application should look like
`apply (relmap phi) decomposeMe`.
Typically, the type of `decomposeMe` is not quite right,
because we have not expressed that `b * Bot` is the same as `b`.
This is where the function `isomorphic` can be used to obtain the result
by simply using `apply (relmap phi) (isomorphic decomposeMe)`.

> relmap :: (AllValues c, AllValues m, AllValues b, AllValues d, 
>            Ord a, Ord b, Ord c, Ord d, Ord m, Show m ) =>
>     RelFunction a b c d -> RelFunction a (m * b) c (m * d)
> relmap (Constant name rel)   = Constant (name ++ pi2Text) (rel .*. transposition pi2)
> relmap Id                    = Id
> relmap (WithBinary op rf sf) = WithBinary op (relmap rf) (relmap sf)
> relmap (Complement rf)       = Complement (relmap rf)
> relmap (LProd x  rf)         = LProd x (relmap rf)
> relmap (RProd rf y)          = RProd (relmap rf) (i .||. y)
> relmap rf                    = 
>   None (\r -> bigsum (\q -> apply rf (r .*. transposition (iotaQ (point q Bot)))))

Textual representation of relational functions
==============================================

In this section we provide functions and constants that can be used to
present relational functions in different text formats.

> pi1Text, pi2Text :: String
> pi1Text = "pi1"
> pi2Text = "pi2"

> unionText, intersectionText, compositionText, transpositionText, constantText,
>  idText, emptyText, largestText, identityText, quasipowerText,
>  unionSymbol, intersectionSymbol, complementText, compositionSymbol,
>  parallelSymbol, injectionText :: String
> unionText          = ".\\/."
> intersectionText   = "./\\."
> compositionText    = ".**."
> transpositionText  = "transpose"
> constantText       = "const"
> idText             = "id"
> emptyText          = "O"
> largestText        = "L"
> identityText       = "I"
> quasipowerText     = "M"
> unionSymbol        = "\\/"
> intersectionSymbol = "/\\"
> complementText     = "complement"
> compositionSymbol  = ".*."
> parallelSymbol     = ".||."
> injectionText      = "iota"

> instance Show BinaryOp where
>   show Union        = unionText
>   show Intersection = intersectionText

> parenPrepend :: String -> String -> String
> parenPrepend pre text = concat [pre, showParen True (showString text) []]

> isSimple :: RelFunction a b c d -> Bool
> isSimple (Constant _ _) = True
> isSimple Id             = True
> isSimple _              = False

> isComposite :: RelFunction a b c d -> Bool
> isComposite = not . isSimple

> instance Show (RelFunction a b c d) where
>   showsPrec _ (Constant name _)   = showString name
>   showsPrec _ Id                  = showString idText
>   showsPrec p (WithBinary op r s) =   showParen (p > precOf op)
>                                     $ showsPrec p r . shows op . showsPrec p s
>   showsPrec p (Complement r)      = showParen (isComposite r) (showsPrec p r)
>   