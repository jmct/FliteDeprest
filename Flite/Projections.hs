module Flite.Projections
    (
      module Flite.Projections.Conversion
    , module Flite.Projections.Contexts
    -- , module Flite.Projections.Projections
    , evalContxt
    , CEnv
    , (##>)
    , (&#)
    , (\/#)
    , meets
    , approxS
    , unfold
    , foldUp
    , getRepeats
    , lubFold
    , primTrans
    , lookupCT
    , deletes
    , kPrime
    , patToCont
    , contToStrat'
    ) where

import Data.List (foldl')
import Data.Maybe (catMaybes, fromMaybe, fromJust)
import Flite.Fresh
import Flite.Syntax
import Flite.Traversals
import Flite.Projections.Conversion
import Flite.Projections.Contexts
import Data.Generics.Uniplate.Direct
import qualified Data.Map.Strict as M

prims :: [String] --List of primitive operators in F-lite
prims = ["(+)", "(-)", "(==)", "(/=)", "(<=)"]

primTrans :: ContextTran
primTrans = M.fromList [ (CBot, CProd [CStr CBot, CStr CBot])
                       , (CProd [], CProd [CStr (CProd []), CStr (CProd [])])
                       ]

isPrim n = n `elem` prims

type CEnv = M.Map String Context

-- Evaluate a given context with the given environment
-- This is for substituting a polymorphic context variable with its context
evalContxt :: CEnv -> Context -> Context
evalContxt p x = transform f x
    where f (CVar n)   = case M.lookup n p of
                            Nothing -> error "Tried to evaluate context without proper substitution"
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


type ContextTran = M.Map Context Context

type FunEnv = M.Map (String, Context) Context

type ValEnv = M.Map String Context

dwn :: Context -> Context
dwn (CLaz c) = c
dwn (CStr c) = c
dwn _        = error "Trying to use dwn on non-lifted context"

-- TODO: I'm not sure about the use of M.Map...
(##>) :: Context -> ValEnv -> ValEnv
(CStr _) ##> k  = k
(CLaz _) ##> k  = M.map (\c -> c \/ mkAbs c) k
c ##> k = error $ "This is the context: " ++ show c

-- Take a recursive context and return its first unfolding
unfold :: Context -> Context
unfold (CSum c)  = CSum c
unfold p@(CMu _ (CSum ds)) = CSum $ mapRange (unfold' p False [""]) ds
unfold _         = error $ "Trying to unfold two contexts of different type"

--         Givenprot -> given -> res Context
unfold' :: Context -> Bool -> [String] -> Context -> Context
unfold' p b cs (CVar m)   = CVar m
unfold' p b cs (CStr m)   = CStr $ unfold' p b cs m
unfold' p b cs (CLaz m)   = CLaz $ unfold' p b cs m
unfold' p b cs (CRec m)   = unfold' p b cs c
  where c = head $ children p
unfold' p b cs CBot       = CBot
unfold' p False cs (CSum bs) = CMu (getBName p ++ "_uf") $ CSum $ mapRange (unfold' p True cs') bs
  where cs' = map fst bs
unfold' p True cs (CSum bs)
    | cs == cs' = CRec "unfolded"
    | otherwise = CSum $ mapRange (unfold' p True cs) bs
  where cs' = map fst bs
unfold' p b cs (CProd bs) = CProd $ map (unfold' p b cs) bs
unfold' p x y z = error "Unfolding malformed context"


lubFold :: Context -> Context -> Context
lubFold (CRec _)   c          = c
lubFold c          (CRec _)   = c
lubFold (CVar a)   (CVar b)   = CVar b
lubFold CBot       c          = c
lubFold c          CBot       = c
lubFold (CProd []) c          = CProd []
lubFold c          (CProd []) = CProd []
lubFold (CSum cs)  (CSum ds)  = if fcs /= fds
                           then error "Performing lubFold on non-equivalent Sum-Contexts"
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


foldUp :: Context -> Context
foldUp x = if rec then res else x
    where rec    = isRec x
          res    = CMu "foldedUP" $ transform f lubbed
          (c:cs) = getRepeats x --Safe because function always returns at least its arg
          lubbed = foldr lubFold c cs
          f (CMu _ c')       = c'
          f y
            | any (y ==) cs = CRec "foldedUp"
            | otherwise     = y

-- Only safe on CSums
-- PROPERTY: head (getRepeats x) == x
getRepeats :: Context -> [Context]
getRepeats c = [ CSum c' | (CSum c') <- universe c, cons' == map fst c']
    where cons' = getCSumNames c

{- This foldup traverses two structures at once...
foldup' p t (CRec n)   = t
foldup' p t (CVar n)   = CVar n
foldup' p t CBot       = CBot
foldup' p t (CProd cs) = undefined

unfoldb' :: Context -> Context -> Context -> Context
unfoldb' x y z = z
unfoldb' p (CVar _) (CVar m)     = CVar m
unfoldb' p (CRec _) (CRec m)     = unfold' p (head $ children p) 
unfoldb' p CBot     CBot         = CBot
unfoldb' p (CSum as) (CSum bs)   = CMu (getBName p ++ "_uf") $ CSum $ zipWRange (unfold' p) as bs
unfoldb' p (CProd as) (CProd bs) = CProd $ zipWith (unfold' p) as bs
unfoldb' p y z = undefined
-}

-- TODO:
-- This should probably go in its own module somewhere
deletes :: Ord k => [k] -> M.Map k a -> M.Map k a
deletes ks m = foldl' (flip M.delete) m ks


-- lookup the context resulting from calling a function with the given demand
lookupCT (env, def) n c = case M.lookup (n, c) env of
                        Just c' -> c'
                        Nothing -> def M.! n -- As long as default is initialised for
                                             -- each function, this is safe.

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
          as'             = zipWith (flip fromMaybe) inList cs
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
lookupPrim ct k        = ct M.! k

prot :: [CDataDec] -> String -> Context
prot decs n = c
  where
    p = cDataCont $ foundIn n decs
    c = fromJust $ lookup n $ concat [ cs | CSum cs <- universe p, n `elem` (map fst cs)]
    -- fromJust is okay since the there not being an constructor of that name is
    -- an error anyway in 'foundIn'

type CompEnv = ([CDataDec], CEnv)

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
    | otherwise = conjs $ zipWith (approxS env phi) (children $ out n $ unfold k) as
approxS env phi k ((Fun n) `App` as)
    | isPrim n  = conjs $ zipWith (approxS env phi) (children $ primTrans `lookupPrim` k) as
    | null as   = M.singleton "ε" $ emptySeq k
    | otherwise = conjs $ zipWith (approxS env phi) (children $ lookupCT (phi, snd env) n k) as
approxS env phi k (Case e alts) = meets newVEnvs
    where newVEnvs        = map (mergeAlt . approxSAlts) alts
          mergeAlt (a, b) = a &# (approxS env phi (foldUp b) e)
          approxSAlts (pat@(App (Con c) as), alt) = (p', k')
            where p' = deletes (freeVars pat) p
                  p  = approxS env phi k alt
                  CProd cs = mkAbs $ prot (fst env) c
                  prod = CProd $ zipWith fromMaybe cs (map (lookupVar p) as)
                  k' = if null as
                       then foldUp $ inC c (CProd []) $ fst env
                       else foldUp $ inC c prod $ fst env
approxS env phi k (Let bs e) = p' &# approxS env phi k' e
    where p' = approxS env phi k e
          k' = undefined
