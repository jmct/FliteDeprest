module Flite.Projections
    (
      module Flite.Projections.Conversion
    , module Flite.Projections.Contexts
    , module Flite.Projections
    ) where

import GHC.Exts (sortWith) -- Why isn't this in Data.List???
import Data.List (foldl', isPrefixOf)
import Data.Maybe (catMaybes, fromMaybe, fromJust)
import Flite.Fresh
import Flite.TypeChecker2
import Flite.TypeUtils
import Flite.Syntax
import Flite.Traversals
import Flite.Dependency
import Flite.Projections.Conversion
import Flite.Projections.Contexts
import Flite.Projections.NiceType
import Data.Generics.Uniplate.Direct
import Flite.Writer
import Control.Applicative
import Control.Monad
import qualified Data.Map.Strict as M
import Debug.Trace

--projAnalysis :: (Prog, [PDataDec], [(Id, Type_exp)]) -> FunEnv
projAnalysis (prog, dataTypes, funTypes) = foldl' f M.empty (take (l - 1) callGs)
  where
    f      = analyseCallGroup (prototypes dataTypes) tMap
    callGs = (fmap . fmap) makeTup (callGroups prog)
    l      = length callGs
    recs   = isSelfRec $ callGraph prog
    ts    = mapRange (retNiceType . toNiceType) funTypes
    tMap = M.fromList ts
    makeTup dec@(Func n _ _) = (dec, fromJust lookR)
      where
        lookR = lookup n recs

type FTypes = M.Map String ([NiceType],NiceType)

analyseCallGroup :: [CDataDec] -> FTypes -> FunEnv -> [(Decl, Bool)] -> FunEnv
analyseCallGroup prots tMap phi [(f@(Func n _ _), isR)]
    | trace ("analysing " ++ n) False = undefined
    | isR       = phi `M.union` (fixMap runRec phi)
    | otherwise = phi `M.union` (M.fromList $ map (runIt phi) conts)
  where 
    (aT, retT) = tMap M.! n
    def2       = botDef prots f aT
    runIt p k  = analyseFunc (f, tMap) (prots, (def2, tMap)) p k
    conts      = pConts prots retT
    getDecl (NCons n ars) = lookupByName n $ prots
    runRec p   = M.fromList $ map (runIt p) conts
analyseCallGroup prots tMap phi decs
    | trace ("analysing " ++ concat (map (funcName . fst) decs)) False = undefined
analyseCallGroup prots tMap phi decs = phi `M.union` (fixMap (go decs') phi)
  where
    decs'       = map fst decs
    ts          = map ((tMap M.!) . funcName) decs' -- types for each function
    defs        = M.unions $ zipWith (\f t -> botDef prots f $ fst t) decs' ts  -- the default context transformers
    go fs p     = M.unions $ map (onEach p) fs
    onEach p f  = p'
      where
        (aT, retT) = tMap M.! (funcName f)
        conts      = pConts prots retT
        p'         = p `M.union` (M.fromList $ map (analyseFunc (f, tMap) (prots, (defs, tMap)) p) conts)

-- Perform one pass of the analysis on a function with one demand context 'k'.
analyseFunc :: (Decl, FTypes) -> CompEnv -> FunEnv -> Context -> ((String, Context), Context)
analyseFunc (Func n as rhs, tMap) env phi k = ((n, k'), mapToProd as defAbs analysisRes)
  where k'           = blankContext k
        (aT, retT)   = tMap M.! n
        defAbs       = map (mkAbs . CLaz . getCont (fst env)) aT
        analysisRes  = approxS env phi k rhs

-- Run a computation on a FunEnv until a fixed point is reached
fixMap :: (FunEnv -> FunEnv) -> FunEnv -> FunEnv
fixMap f p
    | trace ("One iter\n") False = undefined
fixMap f p = let p' = M.union p (f p) in
                 case p' == p of
                   True  -> p'
                   False -> fixMap f p'


-- Create the default context transformer for a function
-- Used as the first iteration when finding a fixed point
botDef :: [CDataDec] -> Decl -> [NiceType] -> M.Map String Context
botDef ps fun = M.singleton (funcName fun) . CProd . map mkStrBot
  where
    mkStrBot = mkBot . CStr . getCont ps

-- Generate all of the principal contexts from a NiceType
pConts :: [CDataDec] -> NiceType -> [Context]
pConts env = allPrinContexts . getCont env

-- Get the prototypical context from a NiceType
getCont :: [CDataDec] -> NiceType -> Context
getCont env (NCons n ars) = case lookupByName n env of
                              Nothing -> if length ars == 0
                                         then CProd []
                                         else error err
                              Just (CData n as rhs) -> let p = zip as $ map (getCont env) ars
                                                       in evalContxt p rhs 
  where 
    err = "Result type of function is not a defined type! This shouldn't happen"
getCont env (NVar a) = CVar a

mapToProd :: [Exp] -> [Context] -> ValEnv -> Context
mapToProd as ds m = CProd res
  where
    l1  = map (lookupVar m) as
    res = zipWith fromMaybe ds l1

prims :: [String] --List of primitive operators in F-lite
prims = ["(+)", "(-)", "(==)", "(/=)", "(<=)"]

primTrans :: M.Map Context Context
primTrans = M.fromList
  [ (CBot,                                        CProd [CStr CBot,       CStr CBot])
  , (CProd [],                                    CProd [CStr (CProd []), CStr (CProd [])])
  , (CSum [("True",CProd []),("False",CProd [])], CProd [CStr (CProd []), CStr (CProd [])])
  , (CSum [("True", CBot),   ("False",CProd [])], CProd [CStr (CBot),     CStr (CBot)])
  , (CSum [("True",CProd []),("False",CBot)],     CProd [CStr (CBot),     CStr (CBot)])
  , (CSum [("True", CBot),   ("False",CBot)],     CProd [CStr (CBot),     CStr (CBot)])
  ]

isPrim n = n `elem` prims

type CEnv = M.Map String Context

-- Evaluate a given context with the given environment
-- This is for substituting a polymorphic context variable with its context
evalContxt :: [(String, Context)] -> Context -> Context
evalContxt p x = transform f x
    where f (CVar n)   = case lookup n p of
                            Nothing -> CVar n
                            Just c -> c
          f c          = c

-- When Looking for contexts in an environment 


(\/#) :: ValEnv -> ValEnv -> ValEnv
x \/# y = M.unions [left, both, right]
  where both  = M.intersectionWith (\/) x y
        left  = M.intersectionWith (\/) x (M.map mkAbs (M.difference x y))
        right = M.intersectionWith (\/) y (M.map mkAbs (M.difference y x))

meets :: [ValEnv] -> ValEnv
meets = foldr1 (\/#)

(&#) :: ValEnv -> ValEnv -> ValEnv
x &# y = M.unionWith (&) x y
{-
    where alike = M.intersectionWith (&) x y
          diffX = M.difference x y
          xConj = M.unionWith (&) diffX (M.map mkBot diffX)
          diffY = M.difference y x
          yConj = M.unionWith (&) diffY (M.map mkBot diffY)
-}

conjs :: [ValEnv] -> ValEnv
conjs = foldr (&#) M.empty



type FunEnv = M.Map (String, Context) Context

type ValEnv = M.Map String Context


-- TODO: I'm not sure about the use of M.Map...
(##>) :: Context -> ValEnv -> ValEnv
(CStr _) ##> k  = k
(CLaz _) ##> k  = M.map (\c -> c \/ mkAbs c) k
c ##> k = error $ "This is the context: " ++ show c

-- Take a recursive context and return its first unfolding
unfold :: Context -> Context
unfold (CSum c)  = CSum c
unfold p@(CMu _ (CSum ds)) = CSum $ mapRange (unfold' p False [""]) ds
unfold c         = c --error $ "Trying to unfold two contexts of different type\n" ++
                     --    show c

--         Givenprot -> given -> res Context
unfold' :: Context -> Bool -> [String] -> Context -> Context
unfold' p b cs (CVar m)   = CVar m
unfold' p b cs (CStr m)   = CStr $ unfold' p b cs m
unfold' p b cs (CLaz m)   = CLaz $ unfold' p b cs m
unfold' p b cs (CRec m)   = unfold' p b cs c
  where c = head $ children p
unfold' p b cs CBot       = CBot
unfold' p@(CMu _ (CSum ds)) False cs (CSum bs)
    | map fst ds == map fst bs = CMu (getBName p ++ "_uf") $ CSum $ mapRange (unfold' p True cs') bs
    | otherwise                = CSum bs
  where cs' = map fst bs
unfold' p True cs (CSum bs)
    | cs == cs' = CRec $ getBName p -- ++ "_uf"
    | otherwise = CSum $ mapRange (unfold' p True cs) bs
  where cs' = map fst bs
unfold' p b cs (CProd bs) = CProd $ map (unfold' p b cs) bs
unfold' p b cs (CMu n c)  = CMu n c
unfold' p x y z = error "Unfolding malformed context"


lubFold :: Context -> Context -> Context
lubFold (CRec n)   (CMu _ c)  = c
lubFold (CRec n)   c          = c
lubFold c          (CRec n)   = CRec n
lubFold (CVar a)   (CVar b)   = CVar b
lubFold CBot       c          = c
lubFold c          CBot       = c
lubFold (CProd []) c          = CProd []
lubFold c          (CProd []) = CProd []
lubFold (CSum cs)  (CSum ds)  = if fcs /= fds
                           then error ("Different sums!\nSum1: " ++ show (CSum cs) ++ "\n\nSum2: " ++ show (CSum ds))
                           else CSum (zipWRange lubFold cs ds)
    where fcs = map fst cs
          fds = map fst ds
lubFold (CProd cs) (CProd ds) = if length cs /= length ds
                           then error "Performing lubFold on non-equivalent Prod-Contexts"
                           else CProd $ zipWith lubFold cs ds
lubFold (CLaz c)   (CLaz d)   = CLaz $ lubFold c d
lubFold (CStr c)   (CLaz d)   = CLaz $ lubFold c d
lubFold (CLaz c)   (CStr d)   = CLaz $ lubFold c d
lubFold (CStr c)   (CStr d)   = CStr $ lubFold c d
lubFold (CMu n c)  (CMu o d)  = CMu n $ reRec n $ lubFold c d
lubFold c          c1         = error $ "Non-exhaustive lubfold on " ++ show c ++ "\n\n" ++ show c1


-- Remove repeated sub-contexts
remReps :: [Context] -> String -> Context -> Context
remReps env n (CSum cs)
    | (CSum cs) `elem` env = CRec n
    | otherwise            = CSum $ mapRange (remReps env n) cs
remReps env n (CProd cs)   = CProd $ map (remReps env n) cs
remReps env n (CMu n' c)   = CMu n' $ remReps env n c
remReps env n (CStr c)     = CStr $ remReps env n c
remReps env n (CLaz c)     = CLaz $ remReps env n c
remReps env n c            = c

-- Fold a context back into it's original recursive form
foldUp :: [CDataDec] -> Context -> Context
--foldUp env x
--    | trace ("\n\nx: " ++ show x) False = undefined
foldUp env x
    | not (isRec x env) = x
foldUp env x@(CSum ds) = CMu n $ remReps cs n lubbed
  where
    t@(CMu n d) = cDataCont $ foundIn (fst $ head ds) env
    lubbed = foldr lubFold c cs
    (c:cs) = getRepeats' t x
foldUp env x = CMu name lubbed
  where
    name   = if null recN then error ("Whoops! this is not recursive!: " ++ show x) else last $ sortWith (suffixCount "_uf") recN
    recN   = [s | CMu s _ <- universe x] ++ [s | CRec s <- universe x]
    (c:cs) = getRepeats name $ CMu name x --Safe because function always returns at least its arg
    lubbed = foldr lubFold c cs

{-
getRepeats' :: Context -> Context -> [Context]
getRepeats' (CMu n x@(CSum cs)) y@(CSum ds)
    = [ CSum cs' | CSum cs' <- universe y, map fst cs' == map fst cs]
    -}

getRepeats' :: Context -> Context -> [Context]
getRepeats' (CMu n x@(CSum cs)) y@(CSum ds) = y : go x y
  where
    go (CRec _)   (CRec _)  = []
    go (CRec _)   (CMu _ d) = [d]
    go (CRec _)   (CSum ds) = [CSum ds]
    go (CMu _ c)  (CMu _ d) = []
    go (CSum cs)  (CMu _ d) = go (CSum cs) d
    go (CSum cs)  (CSum ds) = concat $ zipWith go (map snd cs) (map snd ds)
    go (CProd cs) (CProd ds) = concat $ zipWith go cs ds
    go  c          d
        | isLifted c && isLifted d = go (dwn c) (dwn d)
    go  c          d        = []

-- Only safe on CSums
-- PROPERTY: head (getRepeats x) == x
getRepeats :: String -> Context -> [Context]
getRepeats n c = [ CSum c' | (CMu n' (CSum c')) <- universe c, n == n', cons' == map fst c']
    where cons' = getCSumNames c

{-
-- Count number of times a suffix appears at the end of a string
suffixCount :: String -> String -> Int
suffixCount suf str = length res
  where
    sufLen = length suf
    pre = reverse suf
    res = takeWhile (pre `isPrefixOf`) $ iterate (drop sufLen) $ reverse str

Alternate definition of suffixCount
-}

suffixCount :: String -> String -> Int
suffixCount suf str = go 0 revS
  where
    revS = reverse str
    pre  = reverse suf
    l    = length pre
    go i s
       | pre `isPrefixOf` s = go (i + 1) $ drop l s
       | otherwise          = i


-- TODO:
-- This should probably go in its own module somewhere
deletes :: Ord k => [k] -> M.Map k a -> M.Map k a
deletes ks m = foldl' (flip M.delete) m ks

-- TODO:
-- This seems like a useful function...
firstJust :: b -> [(a -> b, Maybe a)] -> b
firstJust = foldr f
  where
    f (g, Just x)  _ = g x
    f (_, Nothing) y = y


-- lookup the context resulting from calling a function with the given demand
-- Key point here is that the context that we are looking up 'c' may not necessarily
-- be isomorphic to the to context that was inserted in the FunEnv. To deal with this
-- we have to make sure that all of the strings in the context are blanked out 
-- `blankContext` and we have to make sure the context is as general as possible
-- (Defintion 7.6 in Hinze's work)
lookupCT :: (FunEnv, CompEnv) -> String -> Context -> Context
lookupCT (phi, env) n c = firstJust err [ (id,                                 M.lookup (n, blankContext c) phi)
                                        , (evalContxt varMap,                  M.lookup (n, k) phi)
                                        , (evalContxt (mapRange mkBot varMap), M.lookup (n, evalToBot k) phi)
                                        , (id,                                 M.lookup (n, k2) phi)
                                        , (id,                                 M.lookup (n, norm $ blankContext c) phi)
                                        , (id,                                 M.lookup n def)
                                        ]
  where
    def          = fst $ snd env
    retCont      = getCont (fst env) $ snd $ case M.lookup n (snd $ snd env) of { Just x -> x; Nothing -> error "here" }
    prots        = fst env
    k            = blankContext $ c'
    (varMap, c') = getGenCont retCont c
    k2           = blankContext $ getFullBot retCont c
    err          =  error $ "\nTrying to look up\n\n" ++ show (n, blankContext c) ++
                            " in lookupCT\n" ++
                            "\n\n" ++ show (n, k) ++
                            "\n\n" ++ show (n, evalToBot k) ++
                            "\n\n" ++ show (n, k2) ++
                            "\n\n" ++ show (n, norm c)

evalToBot :: Context -> Context
evalToBot = transform f
  where f (CVar _) = CBot
        f c        = c
-- Get un-instantiate a context back to a context with context-variables using a template
-- TODO: re-write this as a Writer monad/applicative
--            template
getGenCont :: Context -> Context -> ([(String, Context)], Context)
getGenCont x y = runWriter $ getGenCont' x y

getGenCont' :: Context -> Context -> Writer (String, Context) Context
getGenCont' (CVar n)    c          = W [(n, c)] (CVar n)
getGenCont' (CRec _)    (CRec n)   = pure $ CRec n
getGenCont' (CSum xs)   (CSum ys)  = do
    let xs' = map snd xs
        ys' = map snd ys
        con = map fst ys
    zs <- zipWithM getGenCont' xs' ys'
    return $ CSum $ zip con zs
getGenCont' (CProd xs)  (CProd ys) = CProd <$> zipWithM getGenCont' xs ys
getGenCont' (CMu _ x)   (CMu n y)  = CMu n <$> getGenCont' x y
getGenCont' x           (CStr y)
    | isLifted x = CStr <$> getGenCont' (dwn x) y
    | otherwise  = error "Template is not matching a lifted context (This should never happen)"
getGenCont' x           (CLaz y)
    | isLifted x = CLaz <$> getGenCont' (dwn x) y
    | otherwise  = error "Template is not matching a lifted context (This should never happen)"
getGenCont' x           (CProd []) = pure $ CProd []
getGenCont' x           CBot       = pure CBot
getGenCont' x           y       = error ("\n\nNon-ex getGenCont'\n\nx: " ++ show x ++ "\n\n" ++ show y)

-- Sometimes the demand on a parameterised type will have 'CBot' in place of a context variable.
-- This function gives you the equivalent 'expanded' context
--            template   scrutinee
getFullBot :: Context -> Context -> Context
getFullBot c CBot
    | c /= CBot && c /= CProd [] = mkBot c
    | otherwise                  = CBot
getFullBot (CVar _) (CVar s) = CVar s    
getFullBot (CRec _) (CRec r) = CRec r
getFullBot (CProd xs) (CProd ys) = CProd $ zipWith getFullBot xs ys
getFullBot (CSum xs)  (CSum ys)  = CSum $ zipWRange getFullBot xs ys
getFullBot (CMu _ x)  (CMu r y)  = CMu r $ getFullBot x y
getFullBot x          y
    | isLifted x && isLifted y = getLift y $ getFullBot (dwn x) (dwn y)
    | otherwise                = error $ ":&:'s and :+:'s should be removed by this point\nx: " ++
                                         show x ++ "\n\ny: " ++ show y


-- Take a context and return the appropriate version for an empty sequence
emptySeq :: Context -> Context
emptySeq CBot       = CBot
emptySeq (CProd []) = CProd []
emptySeq _          = error "Context on the empty sequence that isn't bot or id"


-- Get the ValEnv for the given variables in an expressions
kPrime :: [String] -> ValEnv -> ValEnv
kPrime ns m = M.fromList $ catMaybes $ map f ns
  where f n = case M.lookup n m of
                    Just x  -> Just (n, x)
                    Nothing -> Nothing

-- Form the product from a given pattern
patToCont :: Pat -> Context -> ValEnv -> Context
patToCont (App (Con _) as) (CProd cs) m = CProd $ as'
    where inList          = map getVarC as
          getVarC (Var x) = M.lookup x m
          as'             = zipWith fromMaybe cs inList
patToCont _                _          _ = error "Illegal pattern in patToCont"






contToStrat' :: Context -> Fresh Exp
contToStrat' (CStr (CVar _)) = fresh >>= (\x -> return $ Lam [x] (Var x))
contToStrat' (CLaz (CVar _)) = fresh >>= (\x -> return $ Lam [x] (Con "Unit"))
contToStrat' (CSum cs)       = fresh >>= (\x -> 
                               altsToStrat cs >>= (\cs' -> 
                               return $ Lam [x] (Case (Var x) cs')))
contToStrat' x               = error $ show x

altsToStrat :: [(String, Context)] -> Fresh [Alt]
altsToStrat = mapM altToStrat

altToStrat :: (String, Context) -> Fresh Alt
altToStrat (cName, CBot)     = return $ (App (Con cName) [], Con cName) -- Technically this should be a runtime error
altToStrat (cName, CProd []) = return $ (App (Con cName) [], Con cName)
altToStrat (cName, CProd cs) = freshList (length cs) >>= (\fVars -> 
                               csToFunc fVars cs ([], []) >>= (\cs' ->
                               return $ (App (Con cName) (map Var fVars), App (Con cName) (snd cs')))) --TODO!!!!!!! snd cs is just a placeholder!!!!!!!!
altToStrat _                 = error "The Context passed to altToStrat was malformed"


csToFunc :: [String] -> [Context] -> ([Exp],[Exp]) -> Fresh ([Exp],[Exp])
csToFunc []     []            (ps, es) = return (reverse ps, reverse es)
csToFunc (n:ns) ((CLaz c):cs) (ps, es) = contToStrat' (CLaz c) >>= (\c' -> 
                                         csToFunc ns cs (ps, (App c' [Var n]) : es))
csToFunc (n:ns) ((CStr c):cs) (ps, es) = contToStrat' (CStr c) >>= (\c' ->
                                         csToFunc ns cs ((App (c') [Var n]) : ps, Var n : es))

lookupPrim ct (CVar _) = case M.lookup (CProd []) ct of
                            Just c -> c
                            Nothing -> error $ "This should never happen"
lookupPrim ct k        = case M.lookup k ct of
                            Just c -> c
                            Nothing -> error $ "Primitive lookup failed?! This is the context: " ++ show k

prot :: [CDataDec] -> String -> Context
prot decs n = c
  where
    p = cDataCont $ foundIn n decs
    c = fromJust $ lookup n $ concat [ cs | CSum cs <- universe p, n `elem` (map fst cs)]
    -- fromJust is okay since the there not being an constructor of that name is
    -- an error anyway in 'foundIn'

-- We carry around the prototype contexts and a tuple that holds the default context
-- transformers and the prototype demands for each function.
type CompEnv = ([CDataDec], (CEnv, FTypes))

-- Simple wrapper for looking up variable values in a Map
lookupVar :: M.Map String a -> Exp -> Maybe a
lookupVar m (Var i) = M.lookup i m

approxS :: CompEnv -> FunEnv -> Context -> Exp -> ValEnv
approxS env phi k (Var n)      = M.singleton n k
approxS env phi k (Int n)      = M.empty
approxS env phi k (Con n)      = M.empty
approxS env phi k (Freeze e)   = k ##> approxS env phi (dwn k) e
approxS env phi k (Unfreeze e) = approxS env phi (CStr k) e
approxS env phi k ((Con n) `App` as)
    | null as   = M.singleton "ε" $ emptySeq k
    | otherwise = conjs $ zipWith (approxS env phi) (children $ out' (fst env) n $ unfold k) as
approxS env phi k ((Fun n) `App` as)
    | isPrim n  = conjs $ zipWith (approxS env phi) (children $ primTrans `lookupPrim` k) as
    | null as   = M.singleton "ε" $ emptySeq k
    | otherwise = conjs $ zipWith (approxS env phi) (children $ lookupCT (phi, env) n k) as
approxS env phi k (Case e alts) = meets newVEnvs
    where newVEnvs        = map (mergeAlt . approxSAlts) alts
          mergeAlt (a, b) = a &# (approxS env phi b e)
          approxSAlts (pat@(App (Con c) as), alt) = (p', k')
            where p' = deletes (freeVars pat) p
                  p  = approxS env phi k alt
                  CProd cs = mkAbs $ prot (fst env) c
                  prod = CProd $ zipWith fromMaybe cs (map (lookupVar p) as)
                  k' = if null as
                       then foldUp (fst env) $ inC c (CProd []) $ fst env --TODO: Should it always be CProd []?
                       else foldUp (fst env) $ inC c prod $ fst env
approxS env phi k (Let [(b, e1)] e) = res
    where p   = approxS env phi k e
          p'  = b `M.delete` p
          res  = case b `M.lookup` p of
                      Just v  -> p' &# (v ##> approxS env phi (dwn v) e1)
                      Nothing -> p'
approxS env phi k (Let bs e) = error $ "Static analysis only works on flat Let expressions" ++ show bs
approxS env phit k e         = error $ "Non-exhaustive: " ++ show e

newVEnvs' env phi k (Case e alts) = map (mergeAlt' env phi k e . approxSAlts' env phi k) alts
mergeAlt' env phi k e (a, b) = a &# (approxS env phi b e)
approxSAlts' env phi k (pat@(App (Con c) as), alt) = (p', k')
  where p' = deletes (freeVars pat) p
        p  = approxS env phi k alt
        CProd cs = mkAbs $ prot (fst env) c
        prod = CProd $ zipWith fromMaybe cs (map (lookupVar p) as)
        k' = if null as
             then foldUp (fst env) $ inC c (CProd []) $ fst env --TODO: Should it always be CProd []?
             else foldUp (fst env) $ inC c prod $ fst env
