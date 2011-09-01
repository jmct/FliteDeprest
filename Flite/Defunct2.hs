module Flite.Defunct2 where

import Flite.Fresh
import Flite.Traversals
import Flite.Descend
import Flite.Syntax
import Flite.Pretty

import List
import Debug.Trace



type Request = (Id, Exp)



defunctionalise :: Prog -> Prog
defunctionalise p = trace (show p'') p''
    where
        (p', rqss) = unzip [(Func id args (fst $ traverse defunc rhs), snd $ traverse defunc rhs) | Func id args rhs <- p]
        p'' = case rqs of 
            [] -> p'
            _  -> defunctionalise $ reap $ p' ++ satisfyRequests rqs
        rqs = concat rqss
        defunc = defuncExp p


-- Transform higher-order function applications to first order.
defuncExp :: Prog -> Exp -> (Exp, [Request])
defuncExp p e@( App (Fun id1) ( (Fun id2):as ) ) =
    (App (Fun id1') args', rqs)
    where
        id1' = "^" ++ id1 ++ id2
        args' = as
        rqs = [ (id1', e) ]
defuncExp p e@( App (Fun id1) ( (App (Fun id2) args2):as ) ) =
    (App (Fun id1') args', rqs)
    where
        id1' = "^" ++ id1 ++ id2
        args' = args2 ++ as
        rqs = [ (id1', e) ]
defuncExp p e = (e, [])


-- Create new Decls
satisfyRequest :: Request -> Decl



-- Remove unwanted function definitions
reap :: Prog -> Prog
reap p = reap' ["main"] p

reap' :: Prog -> [Id] -> Prog
reap' p ids = if length ids == ids' then ds else reap' p ids'
    where
        ds = map (lookupFunc p) p
        ids' = nub . concat $ map (nub . calls . funcRhs) ds



-- a bottom-up traversal of an expression, applying a transformation at
--   each stage.
traverse :: ( Exp -> (Exp, [a]) ) -> Exp -> (Exp, [a])
traverse f (App e args) = (exp, xs'')
    where
        (exp, xs) = f (App e' args')
        (e', xs') = traverse f e
        (args', xss) = unzip $ map (traverse f) args
        xs'' = concat (xs:xs':xss)
traverse f (Case e alts) = (exp, xs'')
    where
        (exp, xs) = f (Case e' alts')
        (e', xs') = traverse f e
        (alts', xss) = unzip $ map (traverseAlt f) alts
        xs'' = concat (xs:xs':xss)
traverse f (Let bs e) = (exp, xs'')
    where
        (exp, xs) = f (Let bs' e')
        (bs', xss) = unzip $ map (traverseBinding f) bs
        (e', xs') = traverse f e
        xs'' = concat (xs:xs':xss)
traverse f (Lam is e) = (exp, xs'')
    where
        (exp, xs) = f (Lam is e)
        (e, xs') = traverse f e
        xs'' = xs ++ xs'
traverse f e = f e
    
traverseAlt :: ( Exp -> (Exp, [a]) ) -> Alt -> (Alt, [a])
traverseAlt f (p, e) = ( (p', e'), xs ++ xs')
    where
        (p', xs) = traverse f p
        (e', xs') = traverse f e

traverseBinding :: ( Exp -> (Exp, [a]) ) -> Binding -> (Binding, [a])
traverseBinding f (id, e) = ( (id, e'), xs )
    where
        (e', xs) = traverse f e



