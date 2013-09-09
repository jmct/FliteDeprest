module Flite.Let(
    inlineLinearLet
  , inlineSimpleLet
  , inlinePRSApp
  , liftLet
  , hasNestedPRSApp
  , isPRSArg
  ) where

import Flite.Syntax
import Flite.Traversals
import Flite.Descend
import Flite.Fresh
import Flite.Dependency
import Data.List

mkLet :: [Binding] -> Exp -> Exp
mkLet [] e = e
mkLet bs e = Let bs e

inlineLetWhen :: ([Binding] -> Exp -> Binding -> Bool) -> Prog -> Fresh Prog
inlineLetWhen f p = onExpM freshen p >>= return . onExp inline
  where
    inline (Let bs e) = mkLet (zip vs1 (map inline es1')) (inline e')
      where (vs, es) = unzip bs
            (bs0, bs1) = partition (f bs e) bs
            (vs1, es1) = unzip bs1
            (e':es1') = foldr (\(v, e) -> map (subst e v)) (e:es1)
                          (concat $ letGroups bs0)
    inline e = descend inline e

inlineLinearLet :: Prog -> Fresh Prog
inlineLinearLet p = inlineLetWhen linear p
  where
    linear bs e (v, PRSApp p es) = False
    linear bs e (v, _) = refs v (e:map snd bs) <= 1
    refs v es = sum (map (varRefs v) es)

inlineSimpleLet :: Prog -> Fresh Prog
inlineSimpleLet = inlineLetWhen simple
  where
    simple bs e (v, rhs) = simp rhs
    simp (App e []) = simp e
    simp (App e es) = False
    simp (Case e as) = False
    simp (PRSApp p es) = False
    simp _ = True

inlinePRSApp :: Prog -> Fresh Prog
inlinePRSApp = inlineLetWhen prsApp
  where
    prsApp bs e (v, rhs) =
      isFlatPRSApp rhs && all (not . isPRSArg v) (e:map snd bs)

    isFlatPRSApp (PRSApp p es) = all (not . isPRSApp) es
    isFlatPRSApp e = False

isPRSApp (PRSApp p es) = True
isPRSApp e = False

isPRSArg v (PRSApp p es) = any (isPRSArg' v) es
isPRSArg v (Let bs e)
  | v `elem` map fst bs = False
  | otherwise = any (isPRSArg v) (e:map snd bs)
isPRSArg v (Case e alts) =
  isPRSArg v e ||
    any (isPRSArg v) [e | (p, e) <- alts, v `notElem` patVars p]
isPRSArg v (App e es) = any (isPRSArg v) (e:es)
isPRSArg v (Lam ws e)
  | v `elem` ws = False
  | otherwise = isPRSArg v e
isPRSArg v (PrimApp p es) = any (isPRSArg v) es
isPRSArg v e = False

isPRSArg' v (Var w) = v == w
isPRSArg' v (PRSApp p es) = any (isPRSArg' v) es
isPRSArg' v e = False

hasNestedPRSApp :: Exp -> Bool
hasNestedPRSApp e =
  sum [1 | PRSApp p es <- universe e, any isPRSApp es] > 0

liftLet :: Prog -> Fresh Prog
liftLet p = do p' <- onExpM freshen p
               return (onExp lift p')
  where
    lift e = mkLet [(v, liftInCase rhs) | (v, rhs) <- binds e]
                   (liftInCase (dropBinds e))

    liftInCase (Case e as) = Case e [(p, lift e) | (p, e) <- as]
    liftInCase e = descend liftInCase e

    dropBinds (Let bs e) = dropBinds e
    dropBinds (Case e as) = Case (dropBinds e) as
    dropBinds e = descend dropBinds e

    binds (Let bs e) = binds e ++ [(v, dropBinds e) | (v, e) <- bs]
                               ++ concatMap (binds . snd) bs
    binds (Case e as) = binds e
    binds e = extract binds e
