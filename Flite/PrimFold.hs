module Flite.PrimFold where

import Flite.Syntax
import Flite.Defunct2
import Flite.LambdaLift
import Flite.Traversals
import Flite.Descend


primFold :: Prog -> Prog
primFold p = lambdaLift '^' $ onExp (transformNestedPrimToLambda p) p


transformNestedPrimToLambda :: Prog -> Exp -> Exp
transformNestedPrimToLambda p (App (Fun f) args)
    | isPrimId f = App (Fun f) (map (transformPrimsToLambdas p) $ args)
transformNestedPrimToLambda p e = descend (transformNestedPrimToLambda p) e


transformPrimsToLambdas :: Prog -> Exp -> Exp
transformPrimsToLambdas p (App (Fun f) args)
    | isPrimId f = (App (Lam lids rhs) args)
        where
            rhs = App (Fun f) largs
            largs = getUniqueVars p $ length args
            lids = map getIdOfVar largs
transformPrimsToLambdas p e = e

getIdOfVar :: Exp -> Id
getIdOfVar (Var id) = id



