> {-# LANGUAGE TemplateHaskell #-}
>
> module FiniteTypes ( mkFinite, genericIndex ) where
> 
> import Data.List           ( genericIndex )
> 
> import Language.Haskell.TH ( Q, Dec, Name, Type, Body, Con ( .. ), Exp, mkName, cxt, normalB,
>                              normalC, infixApp, conT, litE, stringL, clause, funD, arithSeqE,
>                              varE, fromToR, instanceD, conP, dataD, appT, appE )


This function creates a new data type with exactly the specified number of defined values.
The datatype is called `N<n>` and has the constructors `N<n>C0, ... N<n>C<n - 1>`.
Additionally this data type is equipped with instances of `Bounded`, 
`Eq`, `Enum` generically, and
`Show`, `AllValues` and `Countable` slightly less generically.

> mkFinite :: Integer -> Q [Dec]
> mkFinite n = sequence
>     [dataD (cxt [])
>            fullName
>            []
>            conss
>            [mkName "Bounded", 
>             mkName "Eq", 
>             mkName "Enum", 
>             mkName "Ord", 
>             mkName "Read", 
>             mkName "Typeable"],
>      sequence conss >>= mkTrivialShowInstance fullName,
>      mkFiniteAllValuesInstance fullName,
>      mkFiniteCountableInstance fullName,
>      mkFiniteInverseCountableInstance fullName]
>     where name     = 'N' : show n
>           fullName = mkName name
>           conss    = map (\i -> normalC (mkName (name ++ "C" ++ show i)) []) [0 .. n - 1]

This function extracts the name of a constructor.

> consName :: Con -> Name
> consName (NormalC n _)   = n
> consName (RecC    n _)   = n
> consName (InfixC _ n _)  = n
> consName (ForallC _ _ c) = consName c

This function is a function composition on an expression level. It can be used to express
`f . g` in Template Haskell. This version is taken from 
[stack overflow](http://stackoverflow.com/questions/9425989/point-free-style-in-template-haskell)

> composeQ :: Q Exp -> Q Exp -> Q Exp
> composeQ = flip infixApp [|(.)|]

This function is a simplified version of an instance declaration. It takes a class name, a type
name and a list of declarations and creates an instance for this class (without any contexts)
for the given type by "writing the declaration list inside the where".

> createInstance :: Name -> Q Type -> [Q Dec] -> Q Dec
> createInstance cName tName = instanceD (cxt []) (appT (conT cName) tName)

This function creates a function with the name funName. The list of constructors and bodies is
zipped one-to-one using `clause`.

> mkFunctionWithConss :: Name -> [Con] -> [Q Body] -> Q Dec
> mkFunctionWithConss funName conss decls =
>     funD funName (zipWith (\cons decl -> clause [conP (consName cons) []] decl []) conss decls)

This function adds a trivial Show instance to a given type with the specified constructors by
simply enumerating the constructors (starting with 0).

> mkTrivialShowInstance :: Name -> [Con] -> Q Dec
> mkTrivialShowInstance tName conss =
>     createInstance
>         (mkName "Show")
>         (conT tName)
>         [mkFunctionWithConss (mkName "show") conss [normalB (litE (stringL (show i))) | i <- [0..]]]

This function creates a simple instance of AllValues by simply defining
      @allValues = [minBound .. maxBound]@
Neither the required Enum context nor the Bounded context are required explicitly.

> mkFiniteAllValuesInstance :: Name -> Q Dec
> mkFiniteAllValuesInstance tName =
>     createInstance
>         (mkName "AllValues")
>         (conT tName)
>         [funD (mkName "allValues")
>               [clause []
>                       (normalB (arithSeqE (fromToR (varE (mkName "minBound")) (varE (mkName "maxBound")) )))
>                       []]]

> mkFiniteCountableInstance :: Name -> Q Dec
> mkFiniteCountableInstance tName = 
>     createInstance
>         (mkName "Countable")
>         (conT tName)
>         [funD (mkName "asInteger") [clause [] (normalB (composeQ (varE (mkName "read")) (varE (mkName "show")))) []]]

> mkFiniteInverseCountableInstance :: Name -> Q Dec
> mkFiniteInverseCountableInstance tName = 
>     createInstance
>         (mkName "InverseCountable")
>         (conT tName)
>         [funD (mkName "intTo") 
>               [clause [] 
>                       (normalB (appE (varE (mkName "genericIndex")) (varE (mkName "allValues"))))
>                       []]]