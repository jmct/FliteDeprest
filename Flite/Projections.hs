module Flite.Projections
    (
      module Flite.Projections.Conversion
    , module Flite.Projections.Contexts
    -- , module Flite.Projections.Projections
    , evalContxt
    , CEnv
    , (##>)
    , (#&)
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

(#&) :: CEnv -> CEnv -> CEnv
x #& y = M.unionWith (&) x y
{-
    where alike = M.intersectionWith (&) x y
          diffX = M.difference x y
          xConj = M.unionWith (&) diffX (M.map mkBot diffX)
          diffY = M.difference y x
          yConj = M.unionWith (&) diffY (M.map mkBot diffY)
-}


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

(##>) :: Context -> ValEnv -> ValEnv
(CStr _) ##> k  = k
(CLaz _) ##> k  = M.map (\c -> c \/ mkAbs c) k

approxS :: FunEnv -> Context -> Exp -> ValEnv
approxS phi k (Var n)      = M.singleton n k
approxS phi k (Int n)      = M.empty
approxS phi k (Freeze e)   = k ##> approxS phi (dwn k) e
approxS phi k (Unfreeze e) = approxS phi (CStr k) e
approxS phi k ((Con n) `App` []) = undefined --ctLookup n k phi
approxS phi k ((Con n) `App` as) = undefined --ctLookup n k phi
approxS phi k ((Fun n) `App` as)
    | isPrim n  = undefined --ctLookup n k phi
    | otherwise = undefined
approxS phi k (Case e alts) = undefined --ctLookup n k phi
approxS phi k (Let bs e) = undefined --ctLookup n k phi



