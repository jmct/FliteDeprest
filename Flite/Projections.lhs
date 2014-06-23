Latest attempt at projections.

> module Flite.Projections where
> import Flite.Syntax
> import Flite.Traversals
> import Flite.Descend


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

And our Type_exp is of the following form

```
data Type_exp = TVAR Tvname
              | TCONS [Char] [Type_exp]
                -- For Projections we need to be able to lift a type
              | Lift Type_exp
              deriving (Eq, Show)
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

> lazifySnd :: [(a, Exp)] -> [(a, Exp)]
> lazifySnd xs = [(x, lazifyExp e) | (x, e) <- xs]

> cleanup :: Exp -> Exp
> cleanup exp = descend f exp
>   where f (Freeze (Unfreeze e)) = e
>         f x = x
The cleanup function is not working...

Then we can lazify an entire program

> lazifyProg :: Prog -> Prog
> lazifyProg fs = [Func name args (cleanup $ lazifyExp rhs) | Func name args rhs <- fs]


