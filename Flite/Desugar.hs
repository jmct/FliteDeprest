module Flite.Desugar (desugar) where

import Flite.LambdaLift
import Flite.Syntax
import Flite.Traversals
import Flite.ConcatApp
import Flite.Matching
import Flite.Case
import Flite.Let
import Flite.Identify
import Flite.Inline
import Flite.Fresh
import Flite.PRSAnalyse
import Control.Monad

desugar :: InlineFlag -> Prog -> Prog
desugar i p = snd (runFresh (frontendM i p) "$" 0)

frontendM :: InlineFlag -> Prog -> Fresh Prog
frontendM i p =
      return (identifyFuncs p)
  >>= desugarCase
  >>= desugarEqn
  >>= return . concatApps
  >>= return . prsAnalyse . liftMain
  >>= inlineTop i
  >>= inlineLinearLet
  >>= inlineSimpleLet
  >>= inlinePRSApp
  >>= return . caseElim
  >>= return . concatApps
  >>= return . lambdaLift 'A'
  >>= inlineTop i
  >>= inlinePRSApp
  >>= inlineLinearLet
  >>= inlineSimpleLet
  >>= liftLet
  >>= return . finalPass

finalPass :: Prog -> Prog
finalPass = map freshen
  where
    freshen (Func f args rhs) = Func f (map Var args') (mkLet bs' e')
      where n = length args
            args' = map (('v':) . show) [0..n-1]
            (bs, e) = body rhs
            (vs, es) = unzip bs
            ws = map (('v':) . show) [n..n+length vs-1]
            from = map var args ++ vs
            to = args' ++ ws
            (e':es') = foldr (\(v, w) -> map (subst (Var w) v))
                             (e:es) (zip from to)
            bs' = zip ws es'

    var (Var v) = v

    body (Let bs e) = (bs, e)
    body e = ([], e)

    mkLet [] e = e
    mkLet bs e = Let bs e

liftMain :: Prog -> Prog
liftMain p = main ++ rest
  where
    rest = [d | d <- p, funcName d /= "main"]
    main = [d | d <- p, funcName d == "main"]

concApps :: Int -> Prog -> Prog
concApps 0 = concatApps
concApps nregs = concatNonPrims
