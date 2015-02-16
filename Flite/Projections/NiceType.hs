module Flite.Projections.NiceType
    ( NiceType(..)
    , toNiceType
    , retNiceType
    ) where

import Flite.Syntax
import Flite.TypeUtils

-- A simple type representation that helps with the projection analysis
data NiceType = NVar String
              | NCons String [NiceType]
              | NArrow NiceType NiceType
         deriving (Show, Eq)

(!) :: Eq a => [(a, b)] -> a -> b
m ! k = case lookup k m of
            Nothing -> error "In this module this is impossible [1]"
            Just v  -> v
{- Note [1]:
        
        Because the map is made up of the Type_exp that we are traversing
        there should never be a Tvname that is not present in the Type_exp.

        This assumes that varMap is implemented correctly. If this ever
        throws an error, then varMap has the bug
 -}

retNiceType :: NiceType -> ([NiceType], NiceType)
retNiceType t = go ([], t)
  where
    go (as, NArrow t1 t2) = go (t1:as, t2)
    go (as, t)            = (reverse as, t)

toNiceType :: Type_exp -> NiceType
toNiceType t = toNiceType' m t
    where m = varMap t

toNiceType' :: [([Int], String)] -> Type_exp -> NiceType
toNiceType' m (TVAR tv)              = NVar $ m ! tv
toNiceType' m (TCONS "TArrow" ts) = NArrow a b
  where
    [a, b] = map (toNiceType' m) ts
toNiceType' m (TCONS c ts)        = NCons c $ map (toNiceType' m) ts
    
