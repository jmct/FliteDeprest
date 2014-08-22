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
    ) where

import Flite.Syntax
import Flite.Projections.Conversion
import Flite.Projections.Contexts
import Data.Generics.Uniplate.Direct
import qualified Data.Map.Strict as M

prims = ["+", "-", "*", "/", ">"]

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
x \/# y = M.unionWith (\/) x y

meets :: [ValEnv] -> ValEnv
meets = foldr (\/#) M.empty

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

type FunEnv = M.Map String ContextTran

ctLookup :: String -> Context -> FunEnv -> Context
ctLookup n c phi = case M.lookup n phi of
                       Nothing -> error "Tried looking up undefined function"
                       Just p -> case M.lookup c p of
                                     Nothing -> error "Context Transformer not defined for Context"
                                     Just c' -> c'

type ValEnv = M.Map String Context

dwn :: Context -> Context
dwn (CLaz c) = c
dwn (CStr c) = c
dwn _        = error "Trying to use dwn on non-lifted context"

-- TODO: I'm not sure about the use of M.Map...
(##>) :: Context -> ValEnv -> ValEnv
(CStr _) ##> k  = k
(CLaz _) ##> k  = M.map (\c -> c \/ mkAbs c) k

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
foldUp x = CMu "foldedUP" $ transform f lubbed
    where (c:cs) = getRepeats x --Safe because function always returns at least its arg
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



approxS :: FunEnv -> Context -> Exp -> ValEnv
approxS phi k (Var n)      = M.singleton n k
approxS phi k (Int n)      = M.empty
approxS phi k (Freeze e)   = k ##> approxS phi (dwn k) e
approxS phi k (Unfreeze e) = approxS phi (CStr k) e
approxS phi k ((Con n) `App` as)
    | null as   = undefined
    | otherwise = conjs $ map (approxS phi $ out n $ unfold k) as
approxS phi k ((Fun n) `App` as)
    | isPrim n  = undefined --ctLookup n k phi
    | otherwise = undefined -- conjs $ map as
approxS phi k (Case e alts) = undefined --meets $ 
    where newVEnvs = map approxSAlts alts
          approxSAlts = undefined
approxS phi k (Let bs e) = undefined --ctLookup n k phi



