module Flite.Syntax where

import Data.Generics.Uniplate.Direct
import Data.Generics.Str

type Prog = [Decl]

data Decl = Func { funcName :: Id
                 , funcArgs :: [Pat]
                 , funcRhs  :: Exp }
            | Data { dataName :: Id
                   , dataArgs :: [Id]
                   , dataCons :: [(Id,[TypeExp])]} deriving (Eq, Show)

type Id = String

data Exp = App Exp [Exp]
         | Case Exp [Alt]
         | Let [Binding] Exp
         | Var Id
         | Con Id
         | Fun Id
         | Int Int

           -- The following may be introduced by various transformations,
           -- but not by the parser.
         | Bottom
         | Alts [Id] Int
         | Ctr Id Int Int
         | Lam [Id] Exp

           -- For speculative evaluation of primitive redexes.
         | PRSApp Id [Exp]   -- guaranteed PRS evaluable (static analysis)
         | PrimApp Id [Exp]  -- candidate for PRS (dynamic testing)
         | Prim Id
           
           -- For Projections we need the following extensions to the AST
         | Freeze Exp
         | Unfreeze Exp
  deriving (Eq,Show)

instance Uniplate Exp where
    uniplate (App e es)  = plate App |* e ||* es
    uniplate (Case e as) = (cs, \str -> let (e':ss) = strList str
                                        in Case e' (zip ps ss))
        where cs = listStr (e:map snd as)
              ps = map fst as
    uniplate (Let bs e)  = (cs, \str -> let (e':ss) = strList str
                                        in Let (zip is ss) e')
        where cs = listStr (e:map snd bs)
              is = map fst bs
    uniplate (Var id)       = plate Var |- id
    uniplate (Con id)       = plate Con |- id
    uniplate (Fun id)       = plate Fun |- id
    uniplate (Int n)        = plate Int |- n
    uniplate (Bottom)       = plate Bottom
    uniplate (Alts is n)    = plate Alts |- is |- n
    uniplate (Ctr i n t)    = plate Ctr |- i |- n |- t
    uniplate (Lam is e)     = plate Lam |- is |* e
    uniplate (PRSApp i es)  = plate PRSApp |- i ||* es
    uniplate (PrimApp i es) = plate PrimApp |- i ||* es
    uniplate (Prim i)       = plate Prim |- i
    uniplate (Freeze e)     = plate Freeze |* e
    uniplate (Unfreeze e)   = plate Unfreeze |* e

type Pat = Exp

type Alt = (Pat, Exp)

type Binding = (Id, Exp)

type App = [Exp]

-- Type Info
type Tvname = [Int]
data Type_exp = TVAR Tvname
              | TCONS [Char] [Type_exp]
                -- For Projections we need to be able to lift a type
              | Lift Type_exp
              deriving (Eq, Show)

data TypeExp =   TEVar String
                |TECons String [TypeExp]
                |TECon  String
              deriving (Eq,Show)



--Stricness Domain Values
data FlatD = T
           | B
        deriving (Eq, Show)

data ListD = FullyStrict
           | SpineStrict
           | InfList
           | BList
        deriving (Eq, Show)

-- Primitive functions

isPrimId :: Id -> Bool
isPrimId p = isBinaryPrim p || isUnaryPrim p

isBinaryPrim :: Id -> Bool
isBinaryPrim "(+)"  = True
isBinaryPrim "(-)"  = True
isBinaryPrim "(==)" = True
isBinaryPrim "(/=)" = True
isBinaryPrim "(<=)" = True
isBinaryPrim _      = False

isUnaryPrim :: Id -> Bool
isUnaryPrim "(!)"  = True
isUnaryPrim "emit" = True
isUnaryPrim "emitInt" = True
isUnaryPrim _ = False

isPredexId :: Id -> Bool
isPredexId = isBinaryPrim

lookupDec :: String -> Prog -> Maybe Decl
lookupDec n [] = Nothing
lookupDec n (d@(Data n' _ _):ds)
    | n == n'   = Just d
    | otherwise = lookupDec n ds
lookupDec n (f@(Func n' _ _):ds)
    | n == n'   = Just f
    | otherwise = lookupDec n ds
