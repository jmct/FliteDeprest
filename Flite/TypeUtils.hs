module Flite.TypeUtils 
    ( varMap -- Get a mapping between [Int]s and Chars
    , varsOf -- Get a list of the variables that appear in a Type_exp
    ) where

import Flite.Syntax
import Data.List (nub)

varMap :: Type_exp -> [([Int],String)]
varMap t = zip (nub $ varsOf t) varNames
  where
  varNames = map (:[]) lett ++ concatMap (\n -> [c : show n | c <- lett]) [1..]
  lett = ['a'..'z']

varsOf :: Type_exp -> [[Int]]
varsOf (TVAR tvn) = [tvn]
varsOf (TCONS tcn ts) = concatMap varsOf ts 

retType :: Type_exp -> Type_exp
retType (TCONS "TArrow" [arg, res]) = retType res
retType x = x
