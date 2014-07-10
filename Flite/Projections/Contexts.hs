module Flite.Projections.Contexts where

import Flite.Projections.Conversion
import Data.Generics.Uniplate.Direct
import Data.Generics.Str



data Context = CVar String
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
 

-- This is not exactly right. It will not give us the correct variations for 'PTEmpty' contexts
allContexts :: Context -> [Context]
allContexts c = concatMap allPrimContexts $ allLiftContexts c

allPrimContexts :: Context -> [Context]
allPrimContexts c = [ f j | (CProd [], f) <- contexts c, j <- [CProd [], CBot]]

allLiftContexts :: Context -> [Context]
allLiftContexts c = [ f j | (CLaz e, f) <- contexts c, j <- [CStr e, CLaz e]]

