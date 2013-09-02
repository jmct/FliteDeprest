module Flite.Syntax where

type Prog = [Decl]

data Decl = Func { funcName :: Id
                 , funcArgs :: [Pat]
                 , funcRhs  :: Exp }
            | Data { dataName :: Id
                   , dataArgs :: [Id]
                 , funcRhs  :: Exp } -- deriving Show

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
  deriving Eq -- deriving (Eq, Show)

type Pat = Exp

type Alt = (Pat, Exp)

type Binding = (Id, Exp)

type App = [Exp]

-- Type Info
type Tvname = [Int]
data Type_exp = TVAR Tvname
              | TCONS [Char] [Type_exp]
              deriving (Eq, Show)

data TypeExp =   TEVar String
                |TECons String [TypeExp]
                |TECon  String
              deriving (Eq,Show)


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
