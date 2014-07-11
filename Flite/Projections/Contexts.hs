module Flite.Projections.Contexts where

import Flite.Projections.Conversion
import Data.Generics.Uniplate.Direct
import Data.Generics.Str
import Data.Maybe (fromMaybe)



data Context = CVar String
             | CRec String
             | CBot
             | CProd [Context]
             | CSum [(String, Context)]
             | CMu String Context
             | CStr Context
             | CLaz Context
         deriving (Show, Eq)

instance Uniplate Context where
    uniplate (CProd cs) = plate CProd ||* cs
    uniplate (CSum cs)  = (listStr (map snd cs), \str -> CSum (zip (map fst cs) (strList str)))
    uniplate (CMu n c)  = plate CMu |- n |* c
    uniplate (CStr c)   = plate CStr |* c
    uniplate (CLaz c)   = plate CLaz |* c
    uniplate c          = (Zero, \Zero -> c)

fromPTExp :: PTExp -> Context
fromPTExp PTEmpty     = CProd []
fromPTExp (PTVar n)   = CVar n
fromPTExp (PTCon n e) = fromPTExp e
fromPTExp (PTSum es)  = CSum $ map f es
    where f (PTCon n e) = (n, fromPTExp e)
fromPTExp (PTProd es) = CProd $ map fromPTExp es
fromPTExp (Mu n e)    = CMu n $ fromPTExp e
fromPTExp (LiftT e)   = CLaz $ fromPTExp e
 

-- Closer to working
allContexts :: Context -> [Context]
allContexts = concatMap allPrimContexts . concatMap allLiftContexts . allVarContexts

allPrimContexts :: Context -> [Context]
allPrimContexts c = concat . take n . iterate (concatMap primContexts) $ [c]
    where n = 1 + length [() | (CProd []) <- universe c]
primContexts :: Context -> [Context]
primContexts c = [ f j | (CProd [], f) <- contexts c, j <- [CBot]]

allLiftContexts :: Context -> [Context]
allLiftContexts c = concat . take n . iterate (concatMap liftContexts) $ [c]
    where n = 1 + length [() | (CLaz e) <- universe c]

liftContexts :: Context -> [Context]
liftContexts c = [ f j | (CLaz e, f) <- contexts c, j <- [CStr e]]

allVarContexts :: Context -> [Context]
allVarContexts c = concat . take n . iterate (concatMap varContexts) $ [c]
    where n = 1 + length [() | (CVar n) <- universe c]

varContexts :: Context -> [Context]
varContexts c = [ f j | (CVar n, f) <- contexts c, j <- [CBot]]

type ImEnv = [(String, Bool)]

-- Identify whether a given context is the bottom context for its type.
improper :: ImEnv -> Context -> Bool
improper e CBot       = True
improper e (CVar n)   = False
improper e (CProd []) = False
improper e (CRec n)   = fromMaybe False $ lookup n e
improper e (CLaz c)   = improper e c
improper e (CStr c)   = True && improper e c
improper e (CSum cs)  = and $ map (improper e . snd) cs
improper e (CProd cs) = or $ map (improper e) cs
improper e (CMu n c)  = improper ((n, True):e) c
