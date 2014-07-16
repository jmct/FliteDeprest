This file takes the standard F-lite core and converts it to a form suitable
for Projections analysis. 

> module Flite.Projections.Conversion 
>     (
>       convertProg -- Take parsed program and return a Triple of:
>                   -- the transformed functions, the expanded and transformed types
>                   -- and the types of top-level functions
>     , PTExp(..)   -- The data-type representing types
>     , PDataDec(..)
>     ) where


> import Flite.Syntax
> import Flite.Traversals
> import Flite.Flic (desugarProj)
> import Flite.TypeChecker2 (tcheck)
> import qualified Flite.Descend as D
> import Data.Maybe (fromMaybe, fromJust) --fromJust used in safe place
> import Data.List (find, intersect, any)
> import Data.Generics.Uniplate.Operations
> import qualified Data.Set as S
> import qualified Data.Map as M


Let's remind ourselves of our AST:


```
data Exp = App Exp [Exp]
         | Case Exp [Alt]
         | Let [Binding] Exp
         | Var Id
         | Con Id
         | Fun Id
         | Int Int

         ...Lots of irrelevant stuff...
           
           -- For Projections we need the following extensions to the AST
         | Freeze Exp
         | Unfreexe Exp
  deriving (Eq,Show)
```

`Freeze` and `Unfreeze` in our AST stand in for primitive functions that
have the type

```
freeze   :: a -> a?
unfreeze :: a? -> a
```

Basically, `Freeze` creates thunks, and `Unfreeze` forces them. Any type with
`?` means that it is a 'lifted' type, which represents a lazy value.


First let's take a normal Flite program and convert it to an equivalent strict
program with explicit thunks. Start with expressions

> lazifyExp :: Exp -> Exp
> lazifyExp (App (Var x) [])= App (Unfreeze (Var x)) []
> lazifyExp (App f args)    = App f $ map (Freeze . lazifyExp) args
> lazifyExp (Var x)         = Unfreeze (Var x)
> lazifyExp (Let bngs exp)  = Let (lazifySnd bngs) (lazifyExp exp)
> lazifyExp (Case exp alts) = Case (lazifyExp exp) (lazifySnd alts)
> lazifyExp _               = error "Static analysis does not support this program!"

> lazifySnd :: [(a, Exp)] -> [(a, Exp)]
> lazifySnd xs = [(x, lazifyExp e) | (x, e) <- xs]

> cleanFreeze :: Exp -> Exp
> cleanFreeze = transform f
>   where f (Freeze (Unfreeze e)) = e -- See Hinze Dis. pg 36 and Sec A.4.1
>         f v                     = v

Then we can lazify an entire program

> lazifyFuncs :: Prog -> Prog
> lazifyFuncs fs = [Func name args (cleanFreeze $ lazifyExp rhs) | Func name args rhs <- fs]


Now we run into some confusion. Flite has two types representing type expressions:

`Type_exp`, which is what we're given from the type checker. And `TypeExp` which is
how data-types are defined. 

Hinze (seems to) have only one representation for both. 

Luckily they don't seem to be too different. 


```
type Tvname = [Int]
data Type_exp = TVAR Tvname
              | TCONS [Char] [Type_exp]
                deriving (Eq, Show)

data TypeExp = TEVar String
             | TECons String [TypeExp]
             | TECon  String
               deriving (Eq,Show)


                -- For Projections we need to be able to lift a type
              | Lift Type_exp
```

The issue is that we need to be able to represent recursive types as u-bound
type variables. This gives us a need to convert our type expressions to the
following form

> data PTExp = PTVar String
>            | PTCon String PTExp -- Invariant: The PTExp must be (PTEmpty | PTProd | PTVar)
>            | PTSum [PTExp]
>            | PTProd [PTExp]
>            | Mu String PTExp
>            | PTEmpty
>            | LiftT PTExp -- Used only for call-by-value
>               deriving (Show, Eq)

When wanting the 'list' of arguments to a constructor we grab the possible values
assuming the invariant above holds.

> getTExpList :: PTExp -> [PTExp]
> getTExpList (PTProd xs) = xs
> getTExpList x           = [x]

> mapOverCons :: (PTExp -> PTExp) -> [PTExp] -> [PTExp]
> mapOverCons f xs = map g xs
>   where g (PTCon n exps) = PTCon n (f exps)
>         g expr           = f expr

So the standard List type could be represented as

```
    PTSum [PTCon "Nil" [], PTCon "Cons" [PTVar "a", PTCon "List" [PTVar "a"]]]
```
Which will need to be transformed into a form using the explicit recursion
operator

```
    Mu "Beta" (PTSum [PTCon "Nil" [], PTCon "Cons" [PTVar "a", PTVar "Beta"]])
```

The reason for this is that the explicit recursion form is closer to the form
of contexts (which the analysis is based on).

Let's convert the data-type declarations to use the `PTExp` type

> data PDataDec = PData { pDataName :: String,
>                         pDataArgs :: [String],
>                         pDataRhs :: PTExp
>                       }
>                   deriving (Show, Eq)

> isRecData :: [PDataDec] -> PDataDec -> Bool
> isRecData ds d = isRecData' ds [] d

> --     All defs-> Names of ADTs seen so far -> ADT in question -> Result
> isRecData' :: [PDataDec] -> [String] -> PDataDec -> Bool 
> isRecData' ds e (PData n as rhs)
>   | null dts             = False
>   | any (`elem` e') dts  = True
>   | otherwise            = or $ map (isRecData' ds e' . getDD ds) dts
>   where dts   = ns `intersect` allCons rhs
>         ns    = map pDataName ds
>         e'    = n:e
>         getDD ds s = fromJust $ find (\(PData n _ _) -> n == s) ds
>                      -- ^ no chance of failure since s always comes from ds

> allCons :: PTExp -> [String]
> allCons (PTCon n expr) = n : allCons expr
> allCons (PTSum exps)   = concatMap allCons exps
> allCons (PTProd exps)  = concatMap allCons exps
> allCons (Mu _ expr)    = allCons expr
> allCons _              = []

> convertDT :: [Decl] -> [PDataDec]
> convertDT dcls = [ PData n args (convertDTRhs rhs)
>                  | Data n args rhs <- dcls ]

> convertDTRhs :: [(String, [TypeExp])] -> PTExp
> convertDTRhs [(name, texp)]  = PTCon name $ PTProd $ map convertTExp texp
> convertDTRhs xs              = PTSum $ map convertCon xs

> convertCon :: (String, [TypeExp]) -> PTExp
> convertCon (name, [])    = PTCon name PTEmpty
> convertCon (name, [exps]) = PTCon name $ PTProd [convertTExp exps]
> convertCon (name, exps)  = PTCon name $ PTProd (map convertTExp exps)

> convertTExp :: TypeExp -> PTExp
> convertTExp (TEVar name)         = PTVar name
> convertTExp (TECons name [])     = PTCon name PTEmpty
> convertTExp (TECons name [texp]) = PTCon name $ convertTExp texp
> convertTExp (TECons name texps)  = PTCon name $ PTProd $ map convertTExp texps
> convertTExp (TECon name)         = PTCon name PTEmpty

Now that we have the Data declarations in a form closer to Hinze's we can
move forward.

Hinze gives us a function that ensures the type expression is in the proper form
(unfix, basically).

> type TEnv = M.Map String PTExp 
> type ExSet = S.Set String

> initTEnv :: TEnv
> initTEnv = M.empty

> initExSet :: ExSet
> initExSet = S.empty

> lookupDef :: String -> [PDataDec] -> Maybe PDataDec
> lookupDef n = find f
>    where f dec = pDataName dec == n

> expand' :: [PDataDec] -> ExSet -> TEnv -> PTExp -> PTExp
> expand' ds u r PTEmpty        = PTEmpty
> expand' ds u r a@(PTVar n)    = fromMaybe a $ M.lookup n r
> expand' ds u r (PTSum cs)     = PTSum $ mapOverCons (expand' ds u r) cs
> expand' ds u r (PTProd cs)    = PTProd $ map (expand' ds u r) cs
> expand' ds u r (PTCon n expr)
>   | n `S.member` u = PTVar n
>   | otherwise      = case lookupDef n ds of
>                          Nothing  -> PTCon n $ expand' ds u r expr
>                          Just def -> expand' ds u' r' rhs
>                            where u'             = n `S.insert` u
>                                  r'             = M.fromAscList $ zip as $ map (expand' ds u r) $ getTExpList expr
>                                  PData n as rhs = def

> expandDef :: [PDataDec] -> PDataDec -> PDataDec
> expandDef ds d@(PData n as rhs)
>   | isRecData ds d = PData n as $ Mu n $ expand' ds (S.singleton n) initTEnv rhs
>   | otherwise = PData n as $ expand' ds initExSet initTEnv rhs

> expandAll :: [PDataDec] -> [PDataDec]
> expandAll ds = map (expandDef ds) ds

Now we can apply the call-by-value transformation to the data-types

> lazifyData :: [PDataDec] -> [PDataDec]
> lazifyData ds = [PData n as (lazifyTExp rhs) | PData n as rhs <- ds]

> lazifyTExp :: PTExp -> PTExp
> lazifyTExp (PTSum exps)      = PTSum $ mapOverCons lazifyTExp exps
> lazifyTExp (PTProd exps)     = PTProd $ mapOverCons (LiftT . lazifyTExp) exps
> lazifyTExp (PTCon n PTEmpty) = PTCon n PTEmpty
> lazifyTExp (PTCon n expr)    = PTCon n $ lazifyTExp expr
> lazifyTExp (Mu n expr)       = Mu n $ lazifyTExp expr
> lazifyTExp expr              = expr

Now we can take a Program and return the lazified functions; the converted,
expanded, and lazified data-types; and the types of the top-level functions

> convertProg :: Prog -> (Prog, [PDataDec], [(Id, Type_exp)])
> convertProg decs = (lFuncs, lData, ts)
>   where lFuncs   = lazifyFuncs $ desugarProj decs
>         (ts, ds) = tcheck decs
>         lData    = lazifyData $ expandAll $ convertDT $ ds
