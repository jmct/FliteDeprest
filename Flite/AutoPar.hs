module Flite.AutoPar where

import Flite.Syntax
import Flite.Writer
import Flite.Projections
import Flite.Projections.Contexts
import qualified Data.Map.Strict as M
import qualified Data.Set as S


-- We represent a 'call-demand' as the name of the function
-- along with the required demand on its result
type Calls = (String, Context)

gatherCalls :: CompEnv -> FunEnv -> Context -> Exp -> Writer Calls ValEnv
gatherCalls env phi k (Var n)      = pure $ M.singleton n k
gatherCalls env phi k (Int n)      = pure $ M.empty
gatherCalls env phi k (Con n)      = pure $ M.empty
gatherCalls env phi k (Freeze e)   = pure $ k ##> gatherCalls env phi (dwn k) e
gatherCalls env phi k (Unfreeze e) = gatherCalls env phi (CStr k) e
gatherCalls env phi k ((Con n) `App` as)
    | null as   = pure $ M.singleton "ε" $ emptySeq k
    | otherwise = fmap conjs $ sequence $ zipWith (gatherCalls env phi) (children $ out' (fst env) n $ unfold k) as
gatherCalls env phi k ((Fun n) `App` as)
    | isPrim n  = write (n, k) >> fmap conjs $ sequence $ zipWith (gatherCalls env phi) (children $ primTrans `lookupPrim` k) as
    | null as   = pure $ M.singleton "ε" $ emptySeq k
    | otherwise = write (n, k') >> fmap conjs $ sequence $ zipWith (gatherCalls env phi) (children $ k') as
  where
    (k', ct) = lookupCT (phi, env) n k
gatherCalls env phi k (Case e alts) = meets newVEnvs
    where newVEnvs        = map (mergeAlt . gatherCallsAlts) alts
          mergeAlt (a, b) = a &# (gatherCalls env phi b e)
          gatherCallsAlts (pat@(App (Con c) as), alt) = (p', k')
            where p' = deletes (freeVars pat) p
                  p  = gatherCalls env phi k alt
                  CProd cs = mkAbs $ prot (fst env) c
                  prod = CProd $ zipWith fromMaybe cs (map (lookupVar p) as)
                  k' = if null as
                       then foldUp (fst env) $ inC c (CProd []) $ fst env --TODO: Should it always be CProd []?
                       else foldUp (fst env) $ inC c prod $ fst env
gatherCalls env phi k (Let [(b, e1)] e) = res
    where p   = gatherCalls env phi k e
          p'  = b `M.delete` p
          res  = case b `M.lookup` p of
                      Just v  -> p' &# (v ##> gatherCalls env phi (dwn v) e1)
                      Nothing -> p'
gatherCalls env phi k (Let bs e) = error $ "Static analysis only works on flat Let expressions" ++ show bs
gatherCalls env phit k e         = error $ "Non-exhaustive: " ++ show e

newVEnvs' env phi k (Case e alts) = map (mergeAlt' env phi k e . gatherCallsAlts' env phi k) alts
mergeAlt' env phi k e (a, b) = a &# (gatherCalls env phi b e)
gatherCallsAlts' env phi k (pat@(App (Con c) as), alt) = (p', k')
  where p' = deletes (freeVars pat) p
        p  = gatherCalls env phi k alt
        CProd cs = mkAbs $ prot (fst env) c
        prod = CProd $ zipWith fromMaybe cs (map (lookupVar p) as)
        k' = if null as
             then foldUp (fst env) $ inC c (CProd []) $ fst env --TODO: Should it always be CProd []?
             else foldUp (fst env) $ inC c prod $ fst env

gatherCalls :: CompEnv -> FunEnv -> Context -> Exp -> (FunEnv, Calls)
--The Boring cases
gatherCalls env phi k (Var n)      = S.empty
gatherCalls env phi k (Int n)      = S.empty
gatherCalls env phi k (Con n)      = S.empty
gatherCalls env phi k (Freeze e)   = gatherCalls env phi (dwn k) e
gatherCalls env phi k (Unfreeze e) = gatherCalls env phi (CStr k) e

-- For constructors we just have to union the call demands from all the sub-expressions
gatherCalls env phi k ((Con n) `App` as)
    | null as   = S.empty
    | otherwise = S.unions $ zipWith (gatherCalls env phi) (children $ out' (fst env) n $ unfold k) as

-- See Note [1]
gatherCalls env phi k ((Fun n) `App` as)
    | isPrim n  = S.unions $ (S.singleton (n,k)) : zipWith (gatherCalls env phi) (children $ primTrans `lookupPrim` k) as
    | null as   = S.empty
    | otherwise = S.unions $ (S.singleton (n,k')) : zipWith (gatherCalls env phi) (children $ ct) as
  where
    (k', ct) = lookupCT' (phi, env) n k

gatherCalls env phi k (Case e alts) = S.unions $ map (mergeAlt . gatherCallsAlts) alts
    where mergeAlt (a, b) = a `S.union` (gatherCalls env phi b e)
          gatherCallsAlts (pat@(App (Con c) as), alt) = (p', k')
            where p' = deletes (freeVars pat) p
                  p  = gatherCalls env phi k alt
                  CProd cs = mkAbs $ prot (fst env) c
                  prod = CProd $ zipWith fromMaybe cs (map (lookupVar p) as)
                  k' = if null as
                       then foldUp (fst env) $ inC c (CProd []) $ fst env --TODO: Should it always be CProd []?
                       else foldUp (fst env) $ inC c prod $ fst env
gatherCalls env phi k (Let [(b, e1)] e) = res
    where p   = gatherCalls env phi k e
          p'  = b `M.delete` p
          res  = case b `M.lookup` p of
                      Just v  -> p' &# (v ##> gatherCalls env phi (dwn v) e1)
                      Nothing -> p'
gatherCalls env phi k (Let bs e) = error $ "Static analysis only works on flat Let expressions" ++ show bs
gatherCalls env phit k e         = error $ "Non-exhaustive: " ++ show e

-----------
-- NOTE [1]
-----------
-- For functions we have to change things up a bit.
-- When the function is primitive, we do what's expected, when it's not we have to make sure we keep
-- the _actual_ context that lookupCT uses to determine the demand on the arguments. For this
-- we have a special version of lookupCT (lookupCT') that returns that context as part of a tuple

-- TODO:
-- This seems like a useful function...
firstJust' :: b -> [(a -> b, c, Maybe a)] -> (c, b)
firstJust' = foldr f
  where
    f (g, k, Just x)  _ = (k, g x)
    f (_, _, Nothing) y = y


-- lookup the context resulting from calling a function with the given demand
-- Key point here is that the context that we are looking up 'c' may not necessarily
-- be isomorphic to the to context that was inserted in the FunEnv. To deal with this
-- we have to make sure that all of the strings in the context are blanked out 
-- `blankContext` and we have to make sure the context is as general as possible
-- (Defintion 7.6 in Hinze's work)
lookupCT' :: (FunEnv, CompEnv) -> String -> Context -> (Context, Context)
lookupCT' (phi, env) n c = firstJust' err [ (id,                                 bc, M.lookup (n, bc) phi)
                                          , (evalContxt varMap,                  k , M.lookup (n, k) phi)
                                          , (evalContxt (mapRange mkBot varMap), k1, M.lookup (n, k1) phi)
                                          , (id,                                 k2, M.lookup (n, k2) phi)
                                          , (id,                                 k3, M.lookup (n, k3) phi)
                                          ]
  where
    def          = fst $ snd env
    retCont      = getCont (fst env) $ snd $ case M.lookup n (snd $ snd env) of { Just x -> x; Nothing -> error "here" }
    prots        = fst env
    bc           = blankContext c
    k            = blankContext $ c'
    k1           = evalToBot k
    k2           = blankContext $ getFullBot retCont c
    k3           = norm $ blankContext c
    (varMap, c') = getGenCont retCont c
    err          =  error $ "\nTrying to look up\n\n" ++ show (n, blankContext c) ++
                            " in lookupCT\n" ++
                            "\n\n" ++ show (n, k) ++
                            "\n\n" ++ show (n, evalToBot k) ++
                            "\n\n" ++ show (n, k2) ++
                            "\n\n" ++ show (n, norm c)
