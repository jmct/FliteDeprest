module Flite.ContextStrat 
       ( isStrict
       , stratefy
       , stratefySCs
       ) where

import Flite.Syntax
import Flite.Fresh
import Flite.Projections
import Flite.Projections.Contexts
import Debug.Trace

{-
 - Let's remind ourselves of the Context ADT:
 -
 - We'll make the constructors that we shouldn't
 - be encountering with XX
 -
 - data Context = CVar String
 -              | CTVar String Context -- XX
 -              | CRec String
 -              | CBot
 -              | CProd [Context] -- Should only be seen as part of CSum
 -              | CSum [(String, Context)]
 -              | CMu String Context
 -              | CStr Context
 -              | CLaz Context
 -              | Context :&: Context -- XX
 -              | Context :+: Context -- XX
 -          deriving (Show, Eq, Ord)
 -}

isStrict :: Context -> Bool
isStrict (CStr _) = True
isStrict _        = False

cUnit = App (Con "Unit") []


stratefySCs :: Int -> Context -> (Int, [(Context, Decl)])
stratefySCs i c = (j, decs)
  where (decs, j, _) = runFreshW (stratefyDec c) "St" i

-- NEW WAY THAT IS SCARY
--
--
--
stratefyDec :: Context -> FreshW (Context, Decl) Exp
stratefyDec c@(CLaz _) = do
    [n,v] <- freshListW 2
    writeF $ (c, Func n [Var v] cUnit)
    return $ Var n
stratefyDec (CStr c) = stratefyDec c
stratefyDec c@(CSum cs) = do
    [f, v] <- freshListW 2
    alts   <- mapM stratefyDecAlt cs
    writeF $ (c, Func f [Var v] (Case (Var v) alts))
    return $ Var f
stratefyDec c@(CMu _ d) = do
    [f,v] <- freshListW 2
    body  <- newScopeW f $ stratefyExp (Var v) d
    writeF $ (c, Func f [Var v] body)
    return $ Var f
stratefyDec (CRec _) = askFreshW >>= return . Var
stratefyDec c = do
    [f, v] <- freshListW 2
    writeF $ (c, Func f [Var v] (App (Fun "seq") [Var v, cUnit]))
    return $ Var f

stratefyExp :: Exp -> Context -> FreshW (Context, Decl) Exp
stratefyExp v (CSum cs) = do
    alts <- mapM stratefyDecAlt cs
    return $ Case v alts

stratefyDecAlt :: (String, Context) -> FreshW (Context, Decl) Alt
stratefyDecAlt (n, c)
    | c == CBot || c == (CProd []) = return $ (App (Con n) [], App (Con "Unit") [])
stratefyDecAlt (n, CProd cs) = do
    ns <- freshListW (length cs)
    let vars  = map Var ns
        contP = filter (isStrict . fst) $ zip cs vars
    rhs <- appChainDec contP
    return (App (Con n) vars, rhs)

appChainDec :: [(Context, Exp)] -> FreshW (Context, Decl) Exp
appChainDec []           = return $ App (Con "Unit") []
appChainDec ((c, v):cvs) = do
    s  <- stratefyDec c
    ss <- appChainDec cvs
    return $ App (Fun "seq") [App s [v], ss]

--
-- FANCY LAMBDA VERSION THAT DOESN'T SEEM TO WORK
--
-- Wrapper to expose pure version
stratefy c = trace s $ runFresh (stratefy' c) "S" 0
  where s = "\n\n" ++ show c


-- Convert a Context to a Strategy
stratefy' :: Context -> Fresh Exp
stratefy' (CLaz _)  = return $ cUnit
stratefy' (CStr c)  = stratefy' c
stratefy' (CMu _ c) = do
    f     <- fresh
    body  <- newScope f $ stratefy' c
    return $ App (Fun "fix") [Lam [f] body]
stratefy' (CRec n)  = askFresh >>= return . Var
stratefy' (CSum cs) = do
    n    <- fresh
    alts <- mapM stratefyAlt cs
    return $ Lam [n] (Case (Var n) alts)
stratefy' c      = fresh >>= (\v -> return $ Lam [v] (App (Fun "seq") [Var v, cUnit]))

stratefyAlt :: (String, Context) -> Fresh Alt
stratefyAlt (n, c)
    | c == CBot || c == (CProd []) = return $ (App (Con n) [], App (Con "Unit") [])
stratefyAlt (n, CProd cs) = do
    ns <- freshList (length cs)
    let vars  = map Var ns
        contP = filter (isStrict . fst) $ zip cs vars
    rhs <- appChain contP
    return (App (Con n) vars, rhs)

appChain :: [(Context, Exp)] -> Fresh Exp
appChain []           = return $ App (Con "Unit") []
appChain ((c, v):cvs) = do
    l  <- stratefy' c
    ls <- appChain cvs
    return $ App (Fun "seq") [App l [v], ls]

