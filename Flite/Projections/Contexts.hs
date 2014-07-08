{-# LANGUAGE DeriveDataTypeable #-}

module Flite.Projections.Contexts where

import Flite.Projections.Conversion
import Data.Generics.Uniplate.Data
import Data.Data



data Context = CVar String
             | CBot
             | CProd [Context]
             | CSum [(String, Context)]
             | CMu String Context
             | CStr Context
             | CLaz Context
         deriving (Show, Eq, Data, Typeable)

fromPTExp :: PTExp -> Context
fromPTExp (PTVar n) = CVar n
fromPTExp (PTCon n exp) =C
 

-- This is not exactly right. It will not give us the correct variations for 'PTEmpty' contexts
allContexts :: Context -> [Context]
allContexts exprs = [ f j | (CVar n, f) <- contexts exprs, j <- [CStr (CVar n), CLaz (CVar n)]]
