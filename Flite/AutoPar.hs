module Flite.AutoPar where

import Flite.Syntax
import Flite.Writer
import Flite.Fresh
import Flite.Traversals
import Flite.Projections hiding (mergeAlt')
import Flite.Projections.Contexts
import Flite.ContextStrat
import Control.Applicative
import Data.Generics.Uniplate.Operations
import Data.Maybe (fromMaybe, fromJust)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List (groupBy, sort)

-- A few utility functions
tfst (x, _, _) = x

-- We represent a 'call-demand' as the name of the function
-- along with the required demand on its result
type Calls = (String, Context)

type BodyCalls = (String, Context, S.Set Calls)

type GathEnv = ([CDataDec], FTypes)

type CallEnv = [(String, M.Map Context String)]

-- Cleanup patterns that don't conform to Wadler's (App p ps) structure

lintEqs :: Prog -> Prog
lintEqs ds = [Func n as (lintEq e) | Func n as e <- ds]

lintEq :: Exp -> Exp
lintEq = transform f
  where
    f (Case e as) = Case e $ map lintEq' as
      where
        lintEq' :: (Pat, Exp) -> (Pat, Exp)
        lintEq' (Con v, e) = (App (Con v) [], e)
        lintEq' eq            = eq
    f exp          = exp

parProg o (p, ds, ts) = parred
  where
    prog                 = lintEqs p
    (pAnal, prots, tMap) = projAnalysis (prog, ds, ts)
    s2                   = gatherProg (prots, tMap) pAnal prog
    dGroups              = demandGroups s2
    (callEnv, renames)   = unzip $ map demSpec dGroups
    prog'                = [Func n as (spec n e) | Func n as e <- cloneFuncsD prog callEnv]
    funDems              = concatMap (flattenDems dGroups) callEnv
    getK n               = fromJustpp $ lookup n funDems
    spec n e             = fst $ specCalls callEnv (prots, tMap) pAnal (getK n) e
    parIt n e            = fst . snd $ runFresh (placePar (concat renames, o) (prots, tMap) pAnal (getK n) e) "P" 0
    parred               = [Func n as (parIt n e) | Func n as e <- prog']

fromJustpp (Just x) = x
fromJustpp Nothing  = error "It was the one in parProg"

-- The ParEnv is where we store the mapping from the specialised functions to their
-- original names (so that we can look them up, and the very important oracle that
-- tells use whether an expression is worth parallelising
type ParEnv = ([(Id, Id)], (Exp -> Bool))

generousOracle :: Exp -> Bool
generousOracle = const True

mediumOracle e = or $ map f (universe e)
  where
    f (App (Fun n) as)
        | n `elem` prims = False
        | otherwise      = True
    f _ = False

mediumOracle' (App (Fun n) as)
    | n `elem` prims = False
    | otherwise      = True
mediumOracle' (Freeze e) = mediumOracle' e
mediumOracle' (Unfreeze e) = mediumOracle' e
mediumOracle' e = False

stingyOracle :: Exp -> Bool
stingyOracle = const False


-- Get the variable names at the corresponding strategies for the
-- arguments that are deemed 'parallelisable' using the oracle + demand
getPars :: (Exp -> Bool) -> [Binding] -> [Context] -> [(Exp, Exp)]
getPars o bs cs = map (formStrat . rExp) ps
    where ps               = filter f $ zip bs cs
          f ((_, e), c)    = (o e) && isStrict c
          rExp ((i, _), c) = (i, c)

formStrat :: (Id, Context) -> (Exp, Exp)
formStrat (v, c) = (Var v, snd $ stratefy c)

makeLet :: [Exp] -> Fresh ([Binding], [Exp])
makeLet as = do
    vs <- freshList (length as)
    let bs = zip vs as
    return $ (bs, map Var vs)


-- Make an Application from a (binding, strategy) pair
mkAp (b, s) = App s [b]

paralleliseApp :: Exp -> [(Exp, Exp)] -> Exp
paralleliseApp fCall []  = fCall
paralleliseApp fCall [b] = App (Fun "par") [mkAp b, fCall]
paralleliseApp fCall bs  = go bs
  where
    go []     = fCall
    go [b]    = App (Fun "seq") [mkAp b, fCall]
    go (b:bs) = App (Fun "par") [mkAp b, go bs]




-- Specialise any functions where the demand on their result requires it
cloneFuncsD :: Prog -> [(Id, M.Map Context Id)] -> Prog
cloneFuncsD p dems = concatMap clone p
  where
    clone f@(Func n as e) = map (\n' -> Func n' as e) newNs
      where
        safeN m = if M.null m then Nothing else Just m
        newNs   = maybe [n] M.elems $ (safeN =<< lookup n dems)
    clone dec           = [dec]

-- Take a nested structure of demands and return a flat assoc-list of function
-- names and the demand on their result
flattenDems :: [(Id, [(Context, S.Set Calls)])] -> (Id, M.Map Context Id) -> [(Id, Context)]
flattenDems dGrps (n, m) = map form fKs
  where
    fKs = map fst $ fromJustfd $ lookup n dGrps
    form k = (fromMaybe n (M.lookup k m), k)

fromJustfd (Just x) = x
fromJustfd Nothing  = error "It was the one in flattenDems"

-- Create a map from the demand on the result of a function to the
-- speciallised name of that function
demSpec :: (Id, [(Context, S.Set Calls)]) -> ((Id, M.Map Context Id), [(Id, Id)])
demSpec (n, [_])                   = ((n, M.empty), [(n,n)])
demSpec (n, xs)
    | (not noCalls || not sameSet) = ((n, m), l)
    | otherwise                    = ((n, M.empty), [(n,n)])
  where
    (m, l)  = specNames n (map fst xs)
    noCalls = and $ map (S.null . snd) xs
    sets    = map snd xs
    sameSet = all ((head sets) ==) sets
    singRec = all (== n) $ fmap fst $ S.toList $ head sets -- See if the function only calls itself

specNames :: String -> [Context] -> (M.Map Context String, [(Id, Id)])
specNames n cs = (M.fromList $ zip cs' ns, zip ns $ repeat n)
  where
    n'   = n ++ "_D"
    nums = map show [1..]
    cs'  = sort cs
    ns   = map (n' ++) $ zipWith const nums cs'

needsDemSpec :: (Id, [(Context, S.Set Calls)]) -> Bool
needsDemSpec (_, [])  = False
needsDemSpec (_, [_]) = False
needsDemSpec (n, xs)  = (not noCalls || not sameSet) && not singRec
  where
    noCalls = and $ map (S.null . snd) xs
    sets    = map snd xs
    sameSet = all ((head sets) ==) sets
    singRec = all (== n) $ fmap fst $ S.toList $ head sets -- See if the function only calls itself

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


--gather1 :: GathEnv -> FunEnv -> Prog -> [(String, Context)] -> S.Set (String, Context)
gatherProg env phi decs = fixSet' (concatMapSet f) (S.singleton (("main", mainDemand), S.empty))
  where
    mainDemand = mkPrimStrict $ getCont (fst env) $ (snd $ fromMaybe err $ M.lookup "main" $ snd env)
    err        = error "Return type for main is not defined?!\nPlease write a main function"
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
    gatherCallsAlts (pat, alt) = error $ show pat
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

-- A traversal of the AST that changes the call to a function to the specialised
-- version
specCalls :: CallEnv -> GathEnv -> FunEnv -> Context -> Exp -> (Exp, ValEnv)
specCalls c env phi k (Var n)      = (Var n, M.singleton n k)
specCalls c env phi k (Int n)      = (Int n, M.empty)
specCalls c env phi k (Con n)      = (Con n, M.empty)
specCalls c env phi k (Freeze e)   = let (e', phi') = specCalls c env phi (dwn k) e
                                     in  (Freeze e', k ##> phi')
specCalls c env phi k (Unfreeze e) = let (e', valEnv) = specCalls c env phi (CStr k) e
                                     in  (Unfreeze e', valEnv)
specCalls c env phi k e@((Con n) `App` as)
    | null as   = (e, M.singleton "ε" $ emptySeq k)
    | otherwise = ((Con n) `App` as', conjs phis)
      where
        res         = zipWith (specCalls c env phi) (children $ out' (fst env) n $ unfold k) as
        (as', phis) = unzip res
specCalls c env phi k ((Fun n) `App` as)
    | isPrim n  = let res = zipWith (specCalls c env phi) (children $ primTrans `lookupPrim` k) as
                      (as', phis) = unzip res
                  in ((Fun n) `App` as', conjs phis)
    | null as   = ((Fun n) `App` [], M.singleton "ε" $ emptySeq k)
    | otherwise = let res =  zipWith (specCalls c env phi) (children $ ct) as
                      (as', phis) = unzip res
                  in ((Fun n') `App` as', conjs phis)
  where
    (k', ct) = lookupCT' (phi, env) n k
    n'       = fromMaybe n (M.lookup k' (fromJust $ lookup n c)) -- n will always be in c
specCalls c env phi k (Case e alts) =
        let (rets , ks)     = unzip $ map specCallsAlts alts
            (alts', fEnvs) = unzip rets
            newK = foldr1 (\/) ks
            (e2, funEnv) = specCalls c env phi newK e
        in (Case e2 alts', (meets fEnvs) \/# funEnv)
  where
    specCallsAlts (pat@(App (Con n) as), alt) =
            let (alt', p) = specCalls c env phi k alt
                p'        = deletes (freeVars pat) p
                CProd cs = mkAbs $ prot (fst env) n
                prod = CProd $ zipWith fromMaybe cs (map (lookupVar p) as)
                k' = if null as
                     then foldUp (fst env) $ inC n (CProd []) $ fst env --TODO: Should it always be CProd []?
                     else foldUp (fst env) $ inC n prod $ fst env
            in (((pat, alt'), p'), k')
specCalls c env phi k (Let [(b, e1)] e) =
        let (e', p) = specCalls c env phi k e
            p'      = b `M.delete` p
        in case b `M.lookup` p of
                Just v  -> let (e1', p1) = specCalls c env phi (dwn v) e1
                           in (Let [(b, e1')] e', p' &# (v ##> p1))
                Nothing -> (Let [(b, e1)] e', p')
specCalls c env phi k (Let bs e) = error $ "Static analysis only works on flat Let expressions" ++ show bs
specCalls c env phit k e         = error $ "Non-exhaustive: " ++ show e

-- A traversal of the AST that actually places the pars
placePar :: ParEnv -> GathEnv -> FunEnv -> Context -> Exp -> Fresh (Exp, ValEnv)
placePar (r, o) env phi k (Var n)      = return $ (Var n, M.singleton n k)
placePar (r, o) env phi k (Int n)      = return $ (Int n, M.empty)
placePar (r, o) env phi k (Con n)      = return $ (Con n, M.empty)
placePar (r, o) env phi k (Freeze e)   = do
                (e', phi') <- placePar (r, o) env phi (dwn k) e
                return $ (Freeze e', k ##> phi')
placePar (r, o) env phi k (Unfreeze e) = do
                (e', phi') <- placePar (r, o) env phi (CStr k) e
                return $ (Unfreeze e', phi')
placePar (r, o) env phi k e@((Con n) `App` as)
    | null as   = return $ (e, M.singleton "ε" $ emptySeq k)
    | otherwise = do
                res <- sequence $ zipWith (placePar (r, o) env phi) (children $ out' (fst env) n $ unfold k) as
                let (as', phis) = unzip res
                return $ ((Con n) `App` as', conjs phis)
placePar (r, o) env phi k ((Fun n) `App` as)
    | isPrim n  = do
                let cs = children $ primTrans `lookupPrim` k
                res <- sequence $ zipWith (placePar (r, o) env phi) cs as
                let (as', phis) = unzip res
                (bs, vs) <- makeLet as'
                let newCall      = (Fun n) `App` vs
                    strats       = getPars o bs cs
                    exp          = paralleliseApp newCall strats
                return $ (Let bs exp, conjs phis)
    | null as   = return $ ((Fun n) `App` [], M.singleton "ε" $ emptySeq k)
    | otherwise = do
                let (k', ct) = lookupCT' (phi, env) n' k
                    n'       = fromMaybe n $ lookup n r
                    cs       = children ct -- Contexts for each argument
                res <- sequence $ zipWith (placePar (r, o) env phi) cs as
                let (as', phis) = unzip res
                (bs, vs) <- makeLet as'
                let newCall      = (Fun n) `App` vs
                    strats       = getPars o bs cs
                    exp          = paralleliseApp newCall strats
                return $ (Let bs exp, conjs phis)
                {-
  where
    (k', ct) = lookupCT' (phi, env) n k
    n'       = fromMaybe n (M.lookup k' (fromJust $ lookup n c)) -- n will always be in c
    -}
placePar (r, o) env phi k (Case e alts) = do
        as <- mapM placeParAlts alts
        let (rets , ks) = unzip as
            (alts', fEnvs) = unzip rets
            newK = foldr1 (\/) ks
        (e2, funEnv) <- placePar (r, o) env phi newK e
        return $ (Case e2 alts', (meets fEnvs) \/# funEnv)
  where
    placeParAlts (pat@(App (Con n) as), alt) = do
                (alt', p) <- placePar (r, o) env phi k alt
                let p' = deletes (freeVars pat) p
                    CProd cs = mkAbs $ prot (fst env) n
                    prod = CProd $ zipWith fromMaybe cs (map (lookupVar p) as)
                    k' = if null as
                         then foldUp (fst env) $ inC n (CProd []) $ fst env --TODO: Should it always be CProd []?
                         else foldUp (fst env) $ inC n prod $ fst env
                return $ (((pat, alt'), p'), k')
placePar (r, o) env phi k (Let [(b, e1)] e) = do
        (e', p) <- placePar (r, o) env phi k e
        let p'      = b `M.delete` p
        case b `M.lookup` p of
                Just v  -> do
                        (e1', p1) <- placePar (r, o) env phi (dwn v) e1
                        return $ (Let [(b, e1')] e', p' &# (v ##> p1))
                Nothing -> return $ (Let [(b, e1)] e', p')
placePar (r, o) env phi k (Let bs e) = error $ "Static analysis only works on flat Let expressions" ++ show bs
placePar (r, o) env phit k e         = error $ "Non-exhaustive: " ++ show e
