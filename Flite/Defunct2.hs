module Flite.Defunct2 where

import Flite.Fresh
import Flite.Traversals
import Flite.Descend
import Flite.Syntax
import Flite.Pretty
import Flite.ConcatApp

import List
import Debug.Trace



type Request = (Id, Exp)
type Replacement = (Exp, Exp) -- (from, to)



defunctionalise :: Prog -> Prog
defunctionalise p = trace (show p'') p''
    where
        (p', rqss) = unzip [(Func id args (fst $ traverse defunc rhs), snd $ traverse defunc rhs) | Func id args rhs <- p]
        p'' = case rqs of 
            [] -> p'
            _  -> defunctionalise $ concatApps $ theEndlessCycleOfDeathAndRebirth rqs p'
        rqs = concat rqss
        defunc = defuncExp p


-- Transform higher-order function applications to first order.
defuncExp :: Prog -> Exp -> (Exp, [Request])
defuncExp p e@( App (Fun id1) ( (Fun id2):as ) )
    | functionExists p id2 && arityOf p id2 > 0 =
        -- trace ("Submitted request: " ++ show rqs) $
        (App (Fun id1') args', rqs)
        where
            id1' = id1 ++ "^" ++ id2
            args' = as
            rqs = [ (id1', e) ]
defuncExp p e@( App (Fun id1) ( (Con id2):as ) ) 
    | arityOfCon p id2 > 0 =
        trace ("Submitted request: " ++ show rqs) $
        (App (Fun id1') args', rqs)
        where
            id1' = id1 ++ "^" ++ id2
            args' = as
            rqs = [ (id1', e) ]
defuncExp p e@( App (Fun id1) ( (App (Fun id2) args2):as ) )
    | functionExists p id2 && arityOf p id2 > length args2 =
        -- trace ("Submitted request: " ++ show rqs) $
        (App (Fun id1') args', rqs)
        where
            id1' = id1 ++ "^" ++ id2
            args' = args2 ++ as
            rqs = [ (id1', e) ]
defuncExp p e@( App (Fun id1) ( (App (Con id2) args2):as ) )
    | arityOfCon p id2 > length args2 =
        (App (Fun id1') args', rqs)
        where
            id1' = id1 ++ "^" ++ id2
            args' = args2 ++ as
            rqs = [ (id1', e) ]
defuncExp p e = (e, [])



theEndlessCycleOfDeathAndRebirth :: [Request] -> Prog -> Prog
theEndlessCycleOfDeathAndRebirth rqs p =
    reaper ["main"] $ p ++ (stork p rqs)



-- Remove unwanted function definitions
reaper :: [Id] -> Prog -> Prog
reaper ids p =
    -- trace (show ids) $
    if length ids == length ids'
        then ds
        else reaper ids' p
    where
        ds = map (lookupFunc p) $ filter (not . isPrimId) ids
        ids' = nub $ ids ++ ( filter (not . isPrimId) $ (concat $ map (calls . funcRhs) ds) )




-- Create a new Decl
stork :: Prog -> [Request] -> Prog
stork p [] = p
stork p ( (id, e):rqs) = stork p' rqs
    where
        p' = d:p
        d = case e of
            ( App (Fun id1) ( (Fun id2):as ) ) ->
                Func id args rhs
                where
                    (Func _ args1 rhs1) = lookupFuncOrPrimitive p id1 
                    (Func _ args2 rhs2) = lookupFuncOrPrimitive p id2
                    args = tail args1
                    repls = [ (head args1, Fun id2),
                              (App (Fun id1) args1, App (Fun id) args) ]
                    rhs = replaceAll repls rhs1
            ( App (Fun id1) ( (Con id2):as ) ) ->
                Func id args rhs
                where
                    (Func _ args1 rhs1) = lookupFuncOrPrimitive p id1 
                    args2 = getArgsForCon $ arityOfCon p id2
                    args = tail args1
                    repls = [ (head args1, Con id2),
                              (App (Fun id1) args1, App (Fun id) args) ]
                    rhs = replaceAll repls rhs1
            ( App (Fun id1) ( app@(App (Fun id2) as2):as1 ) ) ->
                Func id args rhs
                where
                    (Func _ args1 rhs1) = lookupFuncOrPrimitive p id1
                    (Func _ args2 rhs2) = lookupFuncOrPrimitive p id2
                    args2' = take (length as2) args2
                    args = args2' ++ tail args1
                    repls = [ (head args1, App (Fun id2) args2'),
                              (App (Fun id1) args1, App (Fun id) args) ]
                    rhs = replaceAll repls rhs1
            ( App (Fun id1) ( app@(App (Con id2) as2):as1 ) ) ->
                Func id args rhs
                where
                    (Func _ args1 rhs1) = lookupFuncOrPrimitive p id1
                    args2 = getArgsForCon $ arityOfCon p id1
                    args2' = take (length as2) args2
                    args = args2' ++ tail args1
                    repls = [ (head args1, App (Fun id2) args2'),
                              (App (Fun id1) args1, App (Fun id) args) ]
                    rhs = replaceAll repls rhs1
            _ -> error $ "Don't know how to satisfy request for " ++ show (id, e)



-- Some utility functions...

replaceAll :: [Replacement] -> Exp -> Exp
replaceAll rs = fst . traverse (replace rs)

replace :: [Replacement] -> Exp -> ( Exp, [a] )
replace rs exp =
    case subs of 
        [] -> (exp, [])
        [(_, to)] -> (to, [])
        _ -> error $ "Found multiple possible replacements for " ++ show exp ++ ". Not sure what to do!"
    where
        subs = filter ( (exp ==) . fst ) rs

functionExists :: Prog -> Id -> Bool
functionExists p id = isPrimId id || elem id [fid | Func fid _ _ <- p]

lookupFuncOrPrimitive :: Prog -> Id -> Decl
lookupFuncOrPrimitive p id 
    | isBinaryPrim id = Func id args2 (App (Fun id) args2)
    | isUnaryPrim id  = Func id args1 (App (Fun id) args1)
    | otherwise = case lookupFuncs id p of
        [d] -> d
        _ -> error $ "Couldn't find Decl for " ++ id ++ " in \n\n" ++ show p
    where
        args2 = [Var "?a", Var "?b"] 
        args1 = [Var "?c"]


arityOf :: Prog -> Id -> Int
arityOf p id = length args
    where
        Func _ args _ = lookupFuncOrPrimitive p id
    
arityOfCon :: Prog -> Id -> Int
arityOfCon p id = arity
    where
        arity = maximum $ fromExp conArities y
        conArities :: Exp -> [Int]
        conAriies (App (Con cid) args)
            | id == cid = [length args] 
        conArities (Con cid)
            | id == cid = [0]
        conArities e = extract conArities e


getConstructorArity :: Prog -> Id -> [Int]
getConstructorArity p id =
    case fromExp findConPat p of
        (n:ns) -> [maximum (n:ns)]
        [] -> []
    where
        findConPat :: Exp -> [Int]
        findConPat (App (Con cid) args) | cid == id = [length args]
        findConPat (Con cid) | cid == id = [0]
        findConPat e = extract findConPat e


getArgsForCon :: Int -> [Exp]
getArgsForCon 0 = []
getArgsForCon n = (Var $ "?" ++ show (10000 + n)) : (getArgsForCon $ n-1)


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



