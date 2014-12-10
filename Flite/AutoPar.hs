module Flite.AutoPar where

import Flite.Syntax
import Flite.Writer
import Flite.Traversals
import Flite.Projections hiding (mergeAlt')
import Flite.Projections.Contexts
import Control.Applicative
import Data.Generics.Uniplate.Operations
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List (groupBy)

needsDemSpec :: (Id, [(Context, S.Set Calls)]) -> Bool
needsDemSpec (_, [])  = False
needsDemSpec (_, [_]) = False
needsDemSpec (n, xs)  = not noCalls || not sameSet
  where
    noCalls = and $ map (S.null . snd) xs
    sets    = map snd xs
    sameSet = all ((head sets) ==) sets

{- The idea here is to take the raw information from 'gatherProg' and turn it into
 - a more easily processed form. So we pair each function in the program with all
 - of the demands on it and the set of Calls to other functions based on those
 - demands
 -
 -                          (Id, [(Context, S.Set Calls)])
 -             name of func---^      ^        ^ 
 -             demand on result -----|        |----- Calls based on that demand    
 -}
demandGroups :: S.Set ((Id, Context), S.Set Calls) -> [(Id, [(Context, S.Set Calls)])]
demandGroups = uniqIds . uniqDems
  where
    fstEq x y      = fst x == fst y
    nameDem x      = (fst $ head x, S.unions $ map snd x) -- See note 'Head'
    name x y       = (fst $ fst x) == (fst $ fst y)
    drpF ((_,b),c) = (b, c)
    facout x       = (fst $ fst $ head x, map drpF x)     -- See note 'Head'


    -- First factor out the sets of common (Id, Context) pairs
    -- result :: [((Id, Context), S.Set Calls)]
    uniqDems = fmap nameDem . groupBy fstEq . S.toList

    -- Then Factor out the common Ids, putting the varying demands in a list
    -- result :: [(Id, [(Context, S.Set Calls)])]
    uniqIds = fmap facout . groupBy name

        -- NOTE: Head
        -- In these two functions, the use of head is safe because we are mapping
        -- over a list created by 'groupBy'. We know the the 'inner lists' returned by
        -- groupBy are never empty.


{- We aren't at this level yet...
gatherProg :: (Prog, [PDataDec], [(Id, Type_exp)]) -> [Calls]
gatherProg = undefined
-}

setJoin :: Ord a => S.Set (S.Set a) -> S.Set a
setJoin s = S.fold S.union S.empty s

concatMapSet :: (Ord a, Ord b) => (a -> S.Set b) -> S.Set a -> S.Set b
concatMapSet f s = setJoin (S.map f s)

-- Run a computation on a FunEnv until a fixed point is reached
fixSet :: (Ord a, Eq a) => (S.Set a -> S.Set a) -> S.Set a -> S.Set a
--fixSet f p
--    | trace ("One iter\n") False = undefined
fixSet f p = let p' = S.union (f p) p in
                 case p' == p of
                   True  -> p'
                   False -> fixSet f p'

-- Run a computation on a FunEnv until a fixed point is reached
fixSet' :: (Ord a, Eq a) => (S.Set a -> S.Set a) -> S.Set a -> S.Set a
--fixSet' f p
--    | trace ("One iter\n") False = undefined
fixSet' f p = let p' = f p in
                 case p' == p of
                   True  -> p'
                   False -> fixSet' f p'


-- We represent a 'call-demand' as the name of the function
-- along with the required demand on its result
type Calls = (String, Context)

type BodyCalls = (String, Context, S.Set Calls)

type GathEnv = ([CDataDec], FTypes)

--gather1 :: GathEnv -> FunEnv -> Prog -> [(String, Context)] -> S.Set (String, Context)
gatherProg env phi decs = fixSet' (concatMapSet f) (S.singleton (("main", CProd []), S.empty))
  where
    f ((name, k), calls)
        | name `elem` prims = S.empty
        | S.null calls      = S.fromList $ ((name, k), S.fromList calls') : map g calls'
        | otherwise         = S.singleton ((name, k), calls)
      where
        calls' = getCalls env phi k $ getBody name
        g x    = (x, S.empty)

    getBody     = funcRhs . lookupFunc decs

--gather1 :: GathEnv -> FunEnv -> Prog -> [(String, Context)] -> S.Set (String, Context)
gather1 env phi decs = fixSet (concatMapSet f) (S.singleton ("main", CProd []))
  where
    f (name, k)
        | name `elem` prims = S.empty
        | otherwise         = S.fromList $ getCalls env phi k $ getBody name
    getBody     = funcRhs . lookupFunc decs

-- Run a pass of gatherCalls and return only the list of Calls
getCalls :: GathEnv -> FunEnv -> Context -> Exp -> [Calls]
getCalls env phi k = fst . runWriter . gatherCalls env phi k

{-
-- This analysis is close to an exact copy of approxS with a two main differences
--
-- 1) It is in a writer monad, so that we can write the calls to any function
--      as we traverse the Exp
--
-- 2) For Case expressions we only analyse the scrutinee once (instead of once for
--      each alternative). We perform the traversal on the scrutinee with the LUB
--      of the demands on the from each of the alternatives. This gives us the
--      safe demand on the scrutinee
 -}
gatherCalls :: GathEnv -> FunEnv -> Context -> Exp -> Writer Calls ValEnv
gatherCalls env phi k (Var n)      = pure $ M.singleton n k
gatherCalls env phi k (Int n)      = pure $ M.empty
gatherCalls env phi k (Con n)      = pure $ M.empty
gatherCalls env phi k (Freeze e)   = gatherCalls env phi (dwn k) e >>= pure . (k ##>)
gatherCalls env phi k (Unfreeze e) = gatherCalls env phi (CStr k) e
gatherCalls env phi k ((Con n) `App` as)
    | null as   = pure $ M.singleton "ε" $ emptySeq k
    | otherwise = fmap conjs $ sequence $ zipWith (gatherCalls env phi) (children $ out' (fst env) n $ unfold k) as
gatherCalls env phi k ((Fun n) `App` as)
    | isPrim n  = write (n, k) >> (fmap conjs $ sequence $ zipWith (gatherCalls env phi) (children $ primTrans `lookupPrim` k) as)
    | null as   = pure $ M.singleton "ε" $ emptySeq k
    | otherwise = write (n, k') >> (fmap conjs $ sequence $ zipWith (gatherCalls env phi) (children $ ct) as)
  where
    (k', ct) = lookupCT' (phi, env) n k
gatherCalls env phi k (Case e alts) = do
        ts <- sequence $ map gatherCallsAlts alts
        let (fEnvs, ks) = unzip ts
            newK = foldr1 (\/) ks
        gatherCalls env phi newK e >>= (\ funEnv -> pure $ (meets fEnvs) \/# funEnv)
  where
    gatherCallsAlts (pat@(App (Con c) as), alt) = do
            p <- gatherCalls env phi k alt
            let p' = deletes (freeVars pat) p
                CProd cs = mkAbs $ prot (fst env) c
                prod = CProd $ zipWith fromMaybe cs (map (lookupVar p) as)
                k' = if null as
                     then foldUp (fst env) $ inC c (CProd []) $ fst env --TODO: Should it always be CProd []?
                     else foldUp (fst env) $ inC c prod $ fst env
            return (p', k')
gatherCalls env phi k (Let [(b, e1)] e) = do
        p <- gatherCalls env phi k e
        let p'  = b `M.delete` p
        case b `M.lookup` p of
             Just v  -> do
                  p1 <- gatherCalls env phi (dwn v) e1
                  return $ p' &# (v ##> p1)
             Nothing -> return p'
gatherCalls env phi k (Let bs e) = error $ "Static analysis only works on flat Let expressions" ++ show bs
gatherCalls env phit k e         = error $ "Non-exhaustive: " ++ show e

--newVEnvs' env phi k (Case e alts) = map (mergeAlt' env phi k e . gatherCallsAlts' env phi k) alts
-- mergeAlt' env phi k e (a, b) = a &# (gatherCalls env phi b e)
gatherCallsAlts' env phi k (pat@(App (Con c) as), alt) = do
            p <- gatherCalls env phi k alt
            let p' = deletes (freeVars pat) p
                CProd cs = mkAbs $ prot (fst env) c
                prod = CProd $ zipWith fromMaybe cs (map (lookupVar p) as)
                k' = if null as
                     then foldUp (fst env) $ inC c (CProd []) $ fst env --TODO: Should it always be CProd []?
                     else foldUp (fst env) $ inC c prod $ fst env
            return (p', k')

-----------
-- NOTE [1]
-----------
-- For functions we have to change things up a bit.
-- When the function is primitive, we do what's expected, when it's not we have to make sure we keep
-- the _actual_ context that lookupCT uses to determine the demand on the arguments. For this
-- we have a special version of lookupCT (lookupCT') that returns that context as part of a tuple

-- TODO:
-- This seems like a useful function...
firstJust' :: (c, b) -> [(a -> b, c, Maybe a)] -> (c, b)
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
lookupCT' :: (FunEnv, GathEnv) -> String -> Context -> (Context, Context)
lookupCT' (phi, env) n c = firstJust' err [ (id,                                 bc, M.lookup (n, bc) phi)
                                          , (evalContxt varMap,                  k , M.lookup (n, k) phi)
                                          , (evalContxt (mapRange mkBot varMap), k1, M.lookup (n, k1) phi)
                                          , (id,                                 k2, M.lookup (n, k2) phi)
                                          , (id,                                 k3, M.lookup (n, k3) phi)
                                          ]
  where
    retCont      = getCont (fst env) $ snd $ case M.lookup n (snd env) of { Just x -> x; Nothing -> error "here" }
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
