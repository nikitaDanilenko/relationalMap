> {-# Language GADTs, ScopedTypeVariables, TypeOperators, ExplicitNamespaces #-}

> module Relational (
>   RelFunction ( .. ),
>   Rel ( .. ),
>   RelName ( .. ),
>   relmap,
>   apply,
>   toLatex,
>   fromNumbers,
>   fromNumbersFull,
>   fromPairs,
>   relNameToLatex,
>   
>   -- * relational combinators
>   point,
>   i, o, l, pi1, pi2,
>   quasipower,
>   
>   (\/), bigunion,
>   (/\),
>   complement,
>   transposition,
>   (.||.),
>   (.*.),
>   
>   -- * operations on relational functions
>   
>   (.\/.), (./\.), (.**), (**.), (.**.),
>   isomorphic, isomorphic',
>   
>   -- * Miscellaneous
>   type (*)
>   ) where

> import Control.Arrow           ( second, (&&&), (***) )
> import Data.Bits               ( testBit )
> import Data.List               ( intercalate, elemIndex, transpose, genericIndex )
> import Data.Map                ( Map, union, unionWith, intersectionWith, intersection, (\\), empty,
>                                  mapKeys, keys, elems, difference, differenceWith )
> import qualified Data.Map as M ( lookup, fromList, toList )
> import Data.Maybe              ( fromJust, isJust, fromMaybe )
> import Data.Typeable           ( Typeable, typeOf )
>
> import CountingFinite          ( InverseCountable ( intTo ),
>                                  Countable ( asInt, asInteger ), 
>                                  AllValues ( allValues ),
>                                  Bot ( Bot ) )

Composition of functions with different arities: g <.> f :: x -> y -> z; (g <.> f) x y = g (f x y)

> (<.>) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
> (<.>) = (.) . (.)

Unsafe version of a lookup operation.

> fromLookup :: Eq a => a -> [(a, b)] -> b
> fromLookup = fromMaybe undefined <.> lookup 

Creates a function from an association list.
Note that this function is unsafe in the sense that it yields
undefined for values that have no association in the list.

> fromList :: Eq a => [(a, b)] -> a -> b
> fromList = flip fromLookup

Names for binary relational operations

> data BinaryNameOp = Cup | Cap | Comp | ParComp deriving ( Eq )

Precedences of binary relational operations.

> bnoPrec :: BinaryNameOp -> Int
> bnoPrec Cup     = 5
> bnoPrec Cap     = 5
> bnoPrec Comp    = 6
> bnoPrec ParComp = 1

Printed (via show) names for binary relational operations.

> bnoShow :: [(BinaryNameOp, String)]
> bnoShow = [(Cup, unionText),
>            (Cap, intersectionText),
>            (Comp, compositionText),
>            (ParComp, parallelSymbol)]

LaTeX names for binary relational operations.

> bnoLatex :: [(BinaryNameOp, String)]
> bnoLatex = [(Cup, unionLatex),
>            (Cap, intersectionLatex),
>            (Comp, compositionLatex),
>            (ParComp, parallelSymbol)]

> instance Show BinaryNameOp where
>   show = fromList bnoShow

Returns the LaTeX name of a binary relational operation.

> bnoToLatex :: BinaryNameOp -> String
> bnoToLatex = fromList bnoLatex

Unary relational operations.

> data UnaryNameOp = T | C deriving ( Eq )

Printed (via show) names for unary relational operations.

> unoShow :: [(UnaryNameOp, String)]
> unoShow = [(T, transpositionText), (C, complementText)]

> instance Show UnaryNameOp where
>   show = fromList unoShow

Nullary relational operations, i.e. certain constants.
 
> data NullaryNameOp = I String | O String | L String | Pi1 String | Pi2 String deriving ( Eq )

> inParensText :: ShowS -> ShowS
> inParensText s = showString "(" . s . showString ")"

> instance Show NullaryNameOp where
>   show (I t)   = identityText ++ inParensText (showString t) ""
>   show (O t)   = emptyText ++ inParensText (showString t) ""
>   show (L t)   = largestText ++ inParensText (showString t) ""
>   show (Pi1 t) = pi1Text ++ inParensText (showString t) ""
>   show (Pi2 t) = pi2Text ++ inParensText (showString t) ""

> latexSubscript :: String -> String
> latexSubscript s = concat ["_{", s, "}"]

Returns the LaTeX name of a nullary relational operation.

> nnoToLatex :: NullaryNameOp -> String
> nnoToLatex (I t) = concat [identityLatex, latexSubscript t]
> nnoToLatex (O t) = concat [emptyLatex, latexSubscript t]
> nnoToLatex (L t) = concat [largestLatex, latexSubscript t]
> nnoToLatex (Pi1 t) = concat [pi1Latex,  inParens (showString t) ""]
> nnoToLatex (Pi2 t) = concat [pi2Latex, inParens (showString t) ""]

Data type for the names of relations.
Names are either a plain name or a combination of names with certaion relational operations.

> data RelName = Plain String 
>              | Binary BinaryNameOp RelName RelName
>              | Unary UnaryNameOp RelName
>              | Nullary NullaryNameOp

Checks whether a name has no argument.

> isComplexName :: RelName -> Bool
> isComplexName (Plain _)   = False
> isComplexName (Nullary _) = False
> isComplexName _           = True

Shows the name of a function given functions that can show n-ary operation with n <- [0,1,2].

> showWith :: (Bool -> BinaryNameOp -> ShowS -> ShowS -> ShowS)  -- showing binary operations
>          -> (Bool -> UnaryNameOp   -> ShowS -> ShowS)          -- showing unary operations
>          -> (NullaryNameOp -> ShowS)                           -- showing nullary operations
>          -> Int -> RelName -> ShowS
> showWith bshow ushow nshow = go where

>  go _ (Plain name) = showString name
>  go p (Binary bop r s) = bshow (p > bp) bop (go bp r) (go bp s)
>    where bp = bnoPrec bop
>  go _ (Unary uop r)    = ushow (isComplexName r) uop (go 0 r)
>  go _ (Nullary nop)    = nshow nop

> instance Show RelName where
>   showsPrec = showWith (\b bop r s -> showParen b (r . space . shows bop . space . s))
>                        (\b uop r -> shows uop . space . showParen b r)
>                        shows

Print the relational name as a LaTeX string.

> relNameToLatex :: RelName -> String
> relNameToLatex rn = showWith (\b bop r s -> showLatexParen b (
>                                                 r 
>                                               . space 
>                                               . showString (bnoToLatex bop) 
>                                               . space 
>                                               . s))
>                              unary
>                              (showString . nnoToLatex) 0 rn ""
>    where unary _ C r = showString complLatex . inBraces r
>          unary b T r = inBraces (showLatexParen b r) . showString transpLatex

Operations on the data type of relations
========================================

Checks whether an uncurries pair is contained in a relation.

> at :: (Ord a, Ord b) => Rel a b -> a -> b -> Bool
> at (Rel rm _) x y = isJust (M.lookup x rm >>= M.lookup y)

Data type for relations, represented in the adjacency list format.

> data Rel a b = Rel { related :: Map a (Map b ()), symbolicName :: RelName }

Returns the domain of a relation.

> dom :: Rel a b -> [a]
> dom = keys . related

Returns the codomain of a relation.

> cod :: Rel a b -> [b]
> cod = concat . elems . fmap keys . related

Creates a relation from a list of adjacency lists.

> fromPairs :: (Ord a, Ord b) => [(a, [b])] -> RelName -> Rel a b
> fromPairs ps = Rel (M.fromList ( map (\(a, bs) -> (a, M.fromList (map (\b -> (b, ())) bs))) ps ))

The function `fromNumbers` takes a "relation" of indices and transforms the indices into actual 
values.
 
> fromNumbers :: (InverseCountable a, InverseCountable b) => [(Integer, [Integer])] -> [(a, [b])]
> fromNumbers = map (intTo *** map intTo)

Creates a relation from a "relation" of indices.
  
> fromNumbersFull :: (InverseCountable a, InverseCountable b, Ord a, Ord b) 
>                 => [(Integer, [Integer])] -> RelName -> Rel a b
> fromNumbersFull = fromPairs . fromNumbers

Creates a special case of a relational point.
In this case this is a relation that contains exactly one arrow.

> point :: (Ord a, Ord b, Show a, Show b) => a -> b -> Rel a b
> point x y = fromPairs [(x, [y])] (Plain (show (x, y)))

Pretty-prints a relation as a Boolean matrix.

> instance (Show a, Show b, AllValues a, AllValues b, Ord a, Ord b) => Show (Rel a b) where
> 
>     show rf = concat [show (symbolicName rf), "\n",  prettify bvs
>                       , intercalate "\n" [ unwords (show a : map (toBinary a) bvs) | a <- avs ] ]
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

> typeOf1 :: Typeable a => a -> String
> typeOf1 = show . typeOf

> pairType :: String -> String -> String
> pairType left right = concat [left, ", ", right]

> typeOf2 :: (Typeable a, Typeable b) => a -> b -> String
> typeOf2 x y = pairType (typeOf1 x) (typeOf1 y)

The universal relation.

> l :: forall a b . (Typeable a, Typeable b, Ord a, Ord b, AllValues a, AllValues b) => Rel a b
> l = fromPairs [(x, allValues)| x <- allValues] 
>               (Nullary (L (typeOf2 (undefined :: a) (undefined :: b))))

The empty relation.

> o :: forall a b . (Typeable a, Typeable b, AllValues a, AllValues b) => Rel a b
> o = Rel empty (Nullary (O (typeOf2 (undefined :: a) (undefined :: b))))

The identical relation.

> i :: forall a . (Typeable a, Ord a, AllValues a) => Rel a a
> i = fromPairs [(x, [x]) | x <- allValues] (Nullary (I (typeOf1 (undefined :: a))))

The first projection relation. Essentially, this is fst as a relation.

> pi1 :: forall a b . (Ord a, Ord b, AllValues a, AllValues b, Typeable a, Typeable b) => Rel (a * b) a
> pi1 = fromPairs [((x, y), [x]) | x <- allValues, y <- allValues] 
>                 (Nullary (Pi1 (typeOf2 (undefined :: a) (undefined :: b))))

The second projection relation. Essentially, this is snd as a relation.

> pi2 :: forall a b . (Ord a, Ord b, AllValues a, AllValues b, Typeable a, Typeable b) => Rel (a * b) b
> pi2 = fromPairs [((x, y), [y]) | x <- allValues, y <- allValues] 
>                 (Nullary (Pi2 (typeOf2 (undefined :: a) (undefined :: b))))

Returns a powerset-like relation. The "quasi" is simply because p does not need to be a
power type, but can be any type.

> quasipower :: (AllValues a, AllValues p, Countable a, Countable p, Ord a, Ord p) => Rel a p
> quasipower = 
>   fromPairs [(x, [set | set <- allValues, asInteger set `testBit` asInt x]) | x <- allValues] 
>             (Plain quasipowerText)


Relational operations
---------------------

The union of two relations.

> infixr 5 \/
> (\/) :: (Ord a, Ord b) => Rel a b -> Rel a b -> Rel a b
> Rel rm rn \/ Rel sm sn = Rel (unionWith union rm sm) (Binary Cup rn sn)

The union of a list of a relation, which is a folded binary union.

> bigunion :: (Typeable a, Typeable b, AllValues a, AllValues b, Ord a, Ord b) => [Rel a b] -> Rel a b
> bigunion = foldr (\/) o

The intersection of two relations.

> infixr 5 /\
> (/\) :: (Ord a, Ord b) => Rel a b -> Rel a b -> Rel a b
> Rel rm rn /\ Rel sm sn = Rel (intersectionWith intersection rm sm) 
>                              (Binary Cap rn sn)

The complement of a relation.

> complement :: (Typeable a, Typeable b, Ord a, Ord b, AllValues a, AllValues b) => Rel a b -> Rel a b
> complement (Rel rm rn) = Rel (differenceWith (Just <.> difference) (related l) rm)
>                              (Unary C rn)

The transposition of a relation.

> transposition :: (Ord a, Ord b) => Rel a b -> Rel b a
> transposition rel = fromPairs [(y, [ x | x <- d, at rel x y ]) | y <- cod rel]
>                     (Unary T (symbolicName rel))
>    where d = dom rel

The composition of two relations.

> infixr 6 .*.
> (.*.) :: (Ord a, Ord b, Ord c, AllValues b) => Rel a b -> Rel b c -> Rel a c
> f .*. g = fromPairs [ (a , [c | c <- codG, b <- allValues, at f a b && at g b c]) | a <- dom f]
>                     (Binary Comp (symbolicName f) (symbolicName g))
>     where codG = cod g

Parallel composition of relations.
This is the relational version of the function `(***)` from `Control.Arrow`,
where `f *** g = \(x, y) -> (f x, g y)`.

> (.||.) :: (Ord a, Ord b, Ord c, Ord d) => Rel a b -> Rel c d -> Rel (a * c) (b * d)
> f .||. g = fromPairs [((a, c), [(b, d) | b <- cf, d <- cg, at f a b && at g c d]) | a <- dom f, c <- dom g]
>                      (Binary ParComp (symbolicName f) (symbolicName g))
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
>                     (Plain (concat [injectionText, "-", show (symbolicName q)]))

The "theoretical" version of `iotaQ`

> iotaQPure :: (Typeable a, Typeable b, AllValues a, AllValues b, Ord a, Ord b) => Rel b Bot -> Rel a (b * a)
> iotaQPure q = transposition (pi2 /\ pi1 .*. q .*. l)

Injections can be used to express the sum of several relations by placing
relations at different blocks.
The `bigsum` function takes a sum generator and applies it to all points of a type.

> bigsum :: (Typeable a, Typeable b, Typeable m,
>            Ord a, Ord b, Ord m, 
>            AllValues a, AllValues b, AllValues m, Show m) => (m -> Rel a b) -> Rel a (m * b)
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

The constructor `Constant` denotes a constant function and `Id` is the identical function.
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
>    Constant      :: Rel c d -> RelFunction a b c d
>    Id            :: RelFunction a b a b
>    WithBinary    :: BinaryOp -> RelFunction a b c d -> RelFunction a b c d -> RelFunction a b c d
>    Complement    :: RelFunction a b c d -> RelFunction a b c d
>    LProd         :: (Typeable c, AllValues c, Ord c) => Rel e c -> RelFunction a b c d -> RelFunction a b e d
>    RProd         :: (Typeable d, AllValues d, Ord d) => RelFunction a b c d -> Rel d e -> RelFunction a b c e
>    Prod          :: (Typeable d, AllValues d, Ord d) => RelFunction a b c d -> RelFunction a b d e
>                                                                 -> RelFunction a b c e
>    Transposition :: RelFunction a b c d -> RelFunction a b d c
>    None          :: (Rel a b -> Rel c d) -> RelFunction a b c d

Pointwise union of relational functions.

> infixr 4 .\/.
> (.\/.) :: RelFunction a b c d -> RelFunction a b c d -> RelFunction a b c d
> (.\/.) = WithBinary Union

Pointwise intersection of relational functions.

> infixr 5 ./\.
> (./\.) :: RelFunction a b c d -> RelFunction a b c d -> RelFunction a b c d
> (./\.) = WithBinary Intersection

Pointwise left-multiplication with a constant function.
The first argument creates the constant function.

> infixr 6 .**
> (.**) :: (Typeable c, AllValues c, Ord c) => Rel e c -> RelFunction a b c d -> RelFunction a b e d
> (.**) = LProd

Pointwise right-multiplication with a constant function.
The second argument creates the constant function.

> infixr 6 **.
> (**.) :: (Typeable d, AllValues d, Ord d) => RelFunction a b c d -> Rel d e -> RelFunction a b c e
> (**.) = RProd

Pointwise multiplication of relational functions.

> infixr 7 .**.
> (.**.) :: (Typeable d, AllValues d, Ord d) => RelFunction a b c d -> RelFunction a b d e -> RelFunction a b c e
> (.**.) = Prod

Abstract relational functions can be applied to relations.
The apply function transforms the abstract function into a concrete one.

> apply :: (Typeable c, Typeable d, Ord c, Ord d, AllValues c, AllValues d)  => RelFunction a b c d -> Rel a b -> Rel c d
> apply (Constant c)        _ = c
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

> relmap :: (AllValues b, AllValues d, AllValues c, AllValues m, 
>            Ord a, Ord b, Ord c, Ord d, Ord m, 
>            Typeable m, Typeable c, Typeable d,
>            Show m ) =>
>     RelFunction a b c d -> RelFunction a (m * b) c (m * d)
> relmap (Constant rel)        = Constant (rel .*. transposition pi2)
> relmap Id                    = Id
> relmap (WithBinary op rf sf) = WithBinary op (relmap rf) (relmap sf)
> relmap (Complement rf)       = Complement (relmap rf)
> relmap (LProd x  rf)         = LProd x (relmap rf)
> relmap (RProd rf y)          = RProd (relmap rf) (i .||. y)
> relmap (Transposition rf)    = Transposition (
>                                   (Constant (pi1 .*. transposition pi1)
>                                    ./\. 
>                                   (pi2 .** relmap rf)) **. pi2 )
> relmap (Prod rf sf)          = Prod (relmap rf)
>                                     (Constant (pi1 .*. transposition pi1)
>                                      ./\.
>                                      pi2 .** relmap sf)
> relmap rf                    = 
>   None (\r -> bigsum (\q -> apply rf (r .*. transposition (iotaQ (point q Bot)))))

Textual representation of relational functions
==============================================

In this section we provide functions and constants that can be used to
present relational functions in different text formats.

Text variants of all operations and constants. 

> unionText, intersectionText, compositionText, fcompositionText, transpositionText, constantText,
>  idText, emptyText, largestText, identityText, quasipowerText,
>  complementText, parallelSymbol, injectionText, pi1Text, pi2Text :: String
> unionText          = "|_|"
> intersectionText   = "|\"|"
> compositionText    = ".*."
> fcompositionText   = ":*:"
> transpositionText  = "transpose"
> constantText       = "const"
> idText             = "id"
> emptyText          = "O"
> largestText        = "L"
> identityText       = "I"
> quasipowerText     = "M"
> complementText     = "complement"
> parallelSymbol     = "||"
> injectionText      = "iota"
> pi1Text            = "pi1"
> pi2Text            = "pi2"

LaTeX variants of all operations and constants.

> unionLatex, intersectionLatex,
>   funionLatex, fintersectionLatex, compositionLatex, fcompositionLatex, complLatex, fcomplLatex,
>   constantLatex, transpLatex, ftranspLatex, idLatex, emptyLatex, largestLatex, identityLatex, 
>   pi1Latex, pi2Latex :: String
> unionLatex         = "\\cup"
> funionLatex        = "\\sqcup"
> intersectionLatex  = "\\cap"
> fintersectionLatex = "\\sqcap"
> fcompositionLatex  = "\\odot"
> compositionLatex   = "\\cdot"
> constantLatex      = "\\mathsf{const}"
> idLatex            = "\\mathrm{id}"
> emptyLatex         = "\\mathsf O"
> largestLatex       = "\\mathsf L"
> identityLatex      = "\\mathsf I"
> pi1Latex           = "\\pi_1"
> pi2Latex           = "\\pi_2"
> complLatex         = "\\overline"
> fcomplLatex        = "\\mathsf{compl}"
> transpLatex        = "^\\top"
> ftranspLatex       = "\\mathsf{transp}"

Encloses an argument into a given left and right text.

> enclosed :: String -> String -> ShowS -> ShowS
> enclosed left right arg = showString left . arg . showString right

Puts an argument in additional braces.

> inBraces :: ShowS -> ShowS
> inBraces = enclosed "{" "}"

Puts an argument into LaTeX parentheses.

> inParens :: ShowS -> ShowS
> inParens = enclosed "\\left(" "\\right)"

> instance Show BinaryOp where
>   show Union        = unionText
>   show Intersection = intersectionText

Checks whether a relational function is simple, i.e. a constant or identity function.

> isSimple :: RelFunction a b c d -> Bool
> isSimple (Constant _) = True
> isSimple Id           = True
> isSimple _            = False

Negation of `isSimple`.

> isComposite :: RelFunction a b c d -> Bool
> isComposite = not . isSimple

A space as a function.

> space :: ShowS
> space = showString " "

Precedence of the operation of multiplying a function with a constant

> constProdPrec :: Int
> constProdPrec = 6

Maps a binary operation of relational functions to a LaTeX string.

> binaryOpToLatex :: BinaryOp -> String
> binaryOpToLatex Union        = funionLatex
> binaryOpToLatex Intersection = fintersectionLatex

Maps a relational function to a LaTeX representation suited for pasting into a document.
The representation is inductive wherever possible,
thus if the function consists of no application of the `None` constructor,
the result is a closed term without explicit sums as well.

> toLatex :: RelFunction a b c d -> String
> toLatex str = mkLatex 0 str []

The actual function that shows a LaTeX version of a relational function.

> mkLatex :: Int -> RelFunction a b c d -> ShowS
> mkLatex = toStringWith (DC con (showString idLatex) bin com tra pro lpr rpr) where
>   con _ rel      = showConstant rel
>   bin b op rf sf = showLatexParen b (rf . space . showString (binaryOpToLatex op) . space . sf)
>   com _ rf       = showString fcomplLatex . inParens rf
>   tra _ rf       = showString ftranspLatex . inParens rf
>   pro rf sf      = rf . space . showString fcompositionLatex . space . sf
>   lpr b x rf     = showLatexParen b (  showConstant x
>                                      . space 
>                                      . showString fcompositionLatex 
>                                      . space
>                                      . rf )
>   rpr b rf y     = showLatexParen b (  rf
>                                      . space 
>                                      . showString fcompositionLatex 
>                                      . space
>                                      . showConstant y )
>   
>   showConstant rel = showString constantLatex . inParens (showString (relNameToLatex rel))

Parentheses are shown if and only if the Boolean argument is True.

> showLatexParen :: Bool -> ShowS -> ShowS
> showLatexParen b | b         = inParens 
>                  | otherwise = id

> instance Show (RelFunction a b c d) where
>   showsPrec = toStringWith (DC (const shows) (showString idText) bin com tra pro lpr rpr) where
>     bin b op r s = showParen b (r . space . shows op . space . s)
>     com b r      = showString complementText . space . showParen b r
>     tra b r      = showString transpositionText . space . showParen b r
>     pro r s      = r . space . showString fcompositionText . space . s
>     lpr b x r    = showParen b (  showString constantText 
>                                  . showParen True (shows x) 
>                                  . space 
>                                  . showString fcompositionText 
>                                  . space 
>                                  . r )
>     rpr b r y    = showParen b (   r 
>                                  . space 
>                                  . showString fcompositionText 
>                                  . space 
>                                  . showString constantText 
>                                  . showParen True (shows y) )

A capsule containing all functions necessary to transform an abstract relational function
into a formatted string.

> data FunctionCapsule = DC { constFun :: Bool -> RelName -> ShowS,
>                             identFun :: ShowS,
>                             binFun   :: Bool -> BinaryOp -> ShowS -> ShowS -> ShowS,
>                             complFun :: Bool -> ShowS -> ShowS,
>                             transFun :: Bool -> ShowS -> ShowS,
>                             prodFun  :: ShowS -> ShowS -> ShowS,
>                             lprodFun :: Bool -> RelName -> ShowS -> ShowS,
>                             rprodFun :: Bool -> ShowS -> RelName -> ShowS} 

This function takes care of proper parentheses for relational functions.

> toStringWith :: FunctionCapsule
>              -> Int -> RelFunction a b c d -> ShowS
> toStringWith c _ (Constant rel)         = constFun c (isComplexName relName) relName
>   where relName = symbolicName rel
> toStringWith c _ Id                     = identFun c
> toStringWith c p (WithBinary bop rf sf) = binFun c (p > p') 
>                                                  bop
>                                                  (toStringWith c p' rf)
>                                                  (toStringWith c p' sf)
>       where p' = precOf bop
> toStringWith c p (Complement rf)        = complFun c (isComposite rf) (toStringWith c p rf)
> toStringWith c p (Transposition rf)     = transFun c (isComposite rf) (toStringWith c p rf)
> toStringWith c p (Prod rf sf)           = prodFun c (toStringWith c p rf) (toStringWith c p sf)
> toStringWith c p (LProd x rf)           = lprodFun c (p >= constProdPrec) 
>                                                      (symbolicName x)
>                                                      (toStringWith c (min p constProdPrec) rf)
> toStringWith c p (RProd rf y)           = rprodFun c (p >= constProdPrec)
>                                                      (toStringWith c (min p constProdPrec) rf)
>                                                      (symbolicName y)
> toStringWith _ _ (None _)               = showString "Unshowable function."