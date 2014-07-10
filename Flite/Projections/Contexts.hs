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
fromPTExp (PTVar n) = CVar n
fromPTExp (PTCon n exp) =C
 

-- This is not exactly right. It will not give us the correct variations for 'PTEmpty' contexts
allContexts :: Context -> [Context]
allContexts exprs = [ f j | (CVar n, f) <- contexts exprs, j <- [CStr (CVar n), CLaz (CVar n)]]
