module Flite.RedFrontend (frontend) where

import Flite.LambdaLift
import Flite.Syntax
import Flite.Traversals
import Flite.ConcatApp
import Flite.Matching
import Flite.Case
import Flite.Let
import Flite.Identify
import Flite.Strictify
import Flite.Inline
import Flite.Predex
import Flite.Fresh
import Flite.WorkerWrapper
import Control.Monad
import Flite.Pretty
import Flite.PRSAnalyse

import Debug.Trace

frontend :: Bool -> Int -> InlineFlag -> Prog -> Prog
frontend strictAnan nregs i p =
  snd (runFresh (frontendM strictAnan nregs i p) "?" 0)

frontendM :: Bool -> Int -> InlineFlag -> Prog -> Fresh Prog
frontendM strictAnan nregs i p =
  do p0 <- desugarCase (identifyFuncs p) >>= desugarEqn
     let sii = strictIntInfo p0
     p1 <- inlineLinearLet (concatApps p0)
             >>= inlineSimpleLet
             >>= return . lambdaLift 'A'
             >>= return . concApps nregs
             >>= (\p -> return (if strictAnan then workerWrapper sii p else p))
             >>= return . concApps nregs
             >>= inlineTop i
             >>= return . concApps nregs

             >>= return . prsAnalyse . liftMain

             >>= return . caseElimWithCaseStack
             >>= return . concApps nregs
             >>= inlineTop i
             >>= return . concApps nregs
             >>= inlinePRSApp
             >>= inlineLinearLet
             >>= inlineSimpleLet
             >>= return . concApps nregs
             >>= return . identifyPredexCandidates nregs
             >>= return . concatApps
             >>= return . strictifyPrim
             -- >>= return . strictifyPrimOld
             >>= return . concatApps
           --  >>= prsStage
             >>= return . concatApps
     return p1

                 
concApps :: Int -> Prog -> Prog
concApps 0 = concatApps
concApps nregs = concatNonPrims

liftMain :: Prog -> Prog
liftMain p = main ++ rest
  where
    rest = [d | d <- p, funcName d /= "main"]
    main = [d | d <- p, funcName d == "main"]

-- Relating to Static PRS

prsStage :: Prog -> Fresh Prog
prsStage p = 
  do dss <- mapM abstract p
     let p' = concat dss
     if length p == length p' then return p else prsStage p'

abstract :: Decl -> Fresh [Decl]
abstract (Func f xs e) =
  do (bs, e') <- abstr e
     case bs of
       [] -> return [Func f xs e]
       other -> do v <- fresh
                   let (vs, apps) = unzip bs
                   let f' = (f ++ v)
                   let b0 = Func f' (xs ++ map Var vs) e'
                   let b1 = Func f xs (App (Fun f') (xs ++ apps))
                   return [b0, b1]

abstr :: Exp -> Fresh ([Binding], Exp)
abstr (PRSApp p es) = 
  do (bs, es') <- abstrList' es
     return (bs, PRSApp p es')
abstr (App e es) =
   do (bs, e':es') <- abstrList (e:es)
      return (bs, App e' es')
abstr (Let bs e) =
  do (bs0, e0) <- abstr e
     (bs1, es1) <- abstrList (map snd bs)
     return (bs0 ++ bs1, Let (zip (map fst bs) es1) e0)
abstr (PrimApp p es) =
  do (bs, es') <- abstrList es
     return (bs, PrimApp p es')
abstr e = return ([], e)

abstr' (PRSApp p es)
  | all (not . isPRSApp) es =
      do { v <- fresh ; return ([(v, PRSApp p es)], Var v) }
  | otherwise = do (bs, es') <- abstrList' es 
                   return (bs, PRSApp p es')
abstr' e = return ([], e)

abstrList :: [Exp] -> Fresh ([Binding], [Exp])
abstrList es = do ps <- mapM abstr es
                  let bs = concatMap fst ps
                  let es' = map snd ps
                  return (bs, es')

abstrList' :: [Exp] -> Fresh ([Binding], [Exp])
abstrList' es = do ps <- mapM abstr' es
                   let bs = concatMap fst ps
                   let es' = map snd ps
                   return (bs, es')

isPRSApp (PRSApp p es) = True
isPRSApp e = False

{-
abstr :: Exp -> Fresh ([Binding], Exp)
abstr (PRSApp p es)
  | all (not . isPRSApp) es =
      do { v <- fresh ; return ([(v, PRSApp p es)], Var v) }
  | otherwise = do (bs, es') <- abstrList es 
                   return (bs, PRSApp p es')
abstr (App e es) =
   do (bs, e':es') <- abstrList (e:es)
      return (bs, App e' es')
abstr (Case e alts) =
  do (bs0, e0) <- abstr e
     (bs1, es1) <- abstrList (map snd alts)
     return (bs0 ++ bs1, Case e0 (zip (map fst alts) es1))
abstr (Let bs e) =
  do (bs0, e0) <- abstr e
     (bs1, es1) <- abstrList (map snd bs)
     return (bs0 ++ bs1, Let (zip (map fst bs) es1) e0)
abstr (Lam vs e) =
  do (bs, e') <- abstr e
     return (bs, Lam vs e')
abstr (PrimApp p es) =
  do (bs, es') <- abstrList es
     return (bs, PrimApp p es')
abstr e = return ([], e)
-}

