module Flite.IterativeFrontend where

import Flite.Case
import Flite.Desugar
import Flite.Defunct2
import Flite.Identify
import Flite.Matching
import Flite.Syntax
import Flite.Fresh
import Flite.TypeChecker2
import Flite.Flic


--TODO Right now we are only defunctionalising, 
--we need to incorporate type info as well.
convertToCore :: Prog -> Prog
convertToCore = spjCtrNotation

defunc :: Prog -> Prog
defunc p = defunctionalise . snd $ runFresh (passes p) "v_" 0
  where 
    passes prog = desugarEqn (identifyFuncs prog)
        >>= desugarCase
        >>= onExpM freshen
        >>= inlineLinearLet
        >>= inlineSimpleLet
        >>= return . joinApps

--Right now we do not keep the list of Datatype declarations.
--This may need to change depending on how we use the type information
typeC :: Prog -> [(Id, Type_exp)]
typeC = fst . tcheck 
