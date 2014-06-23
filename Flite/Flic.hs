module Flite.Flic (flic, flicM, desugarForG, util) where

import Flite.Syntax
import Flite.Traversals
import Flite.LambdaLift
import Flite.Matching
import Flite.Case
import Flite.Let
import Flite.Identify
import Flite.Fresh
import Flite.Compile
import Flite.Pretty
import Flite.Inline
import Flite.Descend

(!) :: (Eq a, Show a) => [(a, b)] -> a -> b
m ! k =
  case lookup k m of
    Nothing -> error ("Key " ++ show k ++ " not in environment")
    Just v  -> v

desugarForG :: Prog -> Fresh Prog
desugarForG p =
             return (identifyFuncs p)
             >>= desugarEqn
             >>= desugarCase
             >>= onExpM freshen
             >>= inlineLinearLet
             >>= inlineSimpleLet
             >>= return . joinApps

desugarForDandT :: Prog -> Fresh Prog
desugarForDandT p =
             return (identifyFuncs p)
             >>= desugarCase
             >>= desugarEqn

util :: Prog -> Prog
util p = snd $ runFresh (desugarForDandT p) "x" 0

util2 :: Prog -> Prog
util2 p = snd $ runFresh (return (identifyFuncs p) >>= desugarCase) "x" 0

flic :: Prog -> String
flic p = snd (runFresh (flicM p) "v_" 0)

flicM :: Prog -> Fresh String
flicM p =
  do p0 <- return (identifyFuncs p)
             >>= desugarEqn
             >>= desugarCase
             >>= onExpM freshen
             >>= inlineLinearLet
             >>= inlineSimpleLet
             >>= return . joinApps
             >>= return . spjCtrNotation
     return (prettyProg p0)

spjCtrNotation :: Prog -> Prog
spjCtrNotation p = onExp trCtr p
  where
    fs = ctrFamilies p
    m  = [(c, i) | fam <- fs, (c, i) <- zip fam [0..]]

    trCtr (Con c) = Con ("Pack{" ++ show (m!c) ++ ",0}")
    trCtr (App (Con c) es) =
      App (Con ("Pack{" ++ show (m!c) ++ "," ++ show (length es) ++ "}"))
          (map trCtr es)
    trCtr (Case e alts) =
      Case (trCtr e) [(trPat p, trCtr e) | (p, e) <- alts]
    trCtr other = descend trCtr other

    trPat (Con c) = Con ("<" ++ show (m!c) ++ ">")
    trPat other = descend trPat other
