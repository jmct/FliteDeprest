module Flite.Projections.Projections 
       (
         Projection(..)
       ) where

import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Flite.Projections.Conversion
import Flite.Projections.Contexts
import Data.Map (singleton, Map)

data Projection = PVar String
                | PBot
                | PSum [Projection]
                | PProd [Projection]
                | PMu String Projection
                | PStr Projection
                | PLaz Projection
              deriving (Eq, Show)

type PEnv = M.Map String Projection

initPEnv :: PEnv
initPEnv = M.empty

typeToD :: PEnv -> PTExp -> Projection
typeToD e (PTVar n) = fromMaybe (PVar n) $ M.lookup n e
typeToD e (PTCon n PTEmpty) = fromMaybe (PVar n) $ M.lookup n e


nonStd :: Exp -> PFunEnv -> Projection -> PEnv
nonStd (Var n) fs k          = singleton n k
nonStd (Freeze e) fs k       = k #> nonStd e fs (kid k)
nonStd (Unfreeze e) fs k     = nonStd e fs (CStr k)
nonStd (App (Con n) []) fs k = nonStd 

kid :: Projection -> Projection
kid (PStr k) = k
kid (PLaz k) = k
kid _        = error "Trying to take lift off of unlifted projection"

-- The guard operator. See Hinze 7.2
(#>) :: Projection -> PEnv -> PEnv
(PStr k) #> env = undefined


fold = undefined

unfold = undefined

class Eq a => Lattice a where
    top, bot :: a
    (/\), (\/) :: a -> a -> a
    (~<) :: a -> a -> Bool
    x ~< y = (x \/ y) == y 


lfix :: Lattice a => (a -> a) -> a
lfix f = firstRep $ iterate f bot
    where firstRep (x:xs) = if x == head xs then x else firstRep xs
