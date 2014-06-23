Latest attempt at projections.

> module Flite.Projections where
> import Flite.Syntax
> import Flite.Traversals
> import Flite.Descend
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

> cleanup :: Exp -> Exp
> cleanup exp = descend f exp
>   where f (Freeze (Unfreeze e)) = e
>         f x = x

The cleanup function is not working... Import Uniplate?

Then we can lazify an entire program

> lazifyProg :: Prog -> Prog
> lazifyProg fs = [Func name args (cleanup $ lazifyExp rhs) | Func name args rhs <- fs]


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
>            | PTCon String [PTExp]
>            | PTSum [PTExp]
>            | PTProd [PTExp] -- I don't know if I need this...
>            | Mu String PTExp
>               deriving (Show, Eq)

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

> convertDT :: [Decl] -> [PDataDec]
> convertDT dcls = [ PData n args (convertDTRhs rhs)
>                  | Data n args rhs <- dcls ]

> convertDTRhs :: [(String, [TypeExp])] -> PTExp
> convertDTRhs [(name, texp)]  = PTCon name $ map convertTExp texp
> convertDTRhs xs              = PTSum $ map convertCon xs

> convertCon :: (String, [TypeExp]) -> PTExp
> convertCon (name, exp) = PTCon name (map convertTExp exp)

> convertTExp :: TypeExp -> PTExp
> convertTExp (TEVar name)        = PTVar name
> convertTExp (TECons name texps) = PTCon name $ map convertTExp texps
> convertTExp (TECon name)        = PTCon name []

Now that we have the Data declarations in a form closer to Hinze's we can
move forward.

Hinze gives us a function that ensures the type expression is in the proper form
(unfix, basically).

type ...
