module Flite.Pretty where
import Flite.Syntax
import Flite.PrettyLib
import Data.List
import Data.Maybe (fromJust)
	
putProg :: Prog -> IO ()
putProg = putDoc . braces . enclose line line . indent 2 . vsep . punctuate (semi <> line) . map pretty

consperse :: [a] -> [[a]] -> [a]
consperse x xs = concat (intersperse x xs)

prettyProg :: Prog -> String
prettyProg = show . braces . enclose line line . 
             indent 2 . vsep . punctuate (semi <> line) . map pretty

instance Pretty Decl where
	pretty (Func f a r) = nest 2 $ text f
						<+> hsep (map prettyArg a)
						</> char '='
						<+> pretty r
						
instance Pretty Exp where
	pretty (App x ys) 	= hsep (prettyArg2 x : map prettyArg ys)
	pretty (Case x as)	= nest 2 (text "case" </> prettyArg x)
						</> nest 2 (text "of" </> prettyBlock prettyAlt as)
	pretty (Let bs y)	= nest 2 (text "let" </> prettyBlock prettyBind bs)
						</> nest 2 (text "in" </> prettyArg y)
	pretty (Lam vs x)	= nest 2 (text "\\" <> (hsep . map text $ vs) </> text "->" </> pretty x)
	pretty (Var v)		= text v
	pretty (Fun f)		= text f
	pretty (Con c)		= text c
	pretty (Int i)		= int i
	pretty (Bottom)		= text "Undef"

---Print types
instance Show Type_exp where
  show t =  showWith id (varMap t) t

showWith :: (String -> String) -> [([Int],String)] -> Type_exp -> String
showWith b m (TVAR tvn)      = (fromJust $ lookup tvn m) 
                                --"V" ++ (concat $ map show  tvn )--
showWith b m (TCONS tcn ts)  = 
     case tcn of
     "TArrow" ->  b $ showWith brack m t1 ++ " -> " ++ showWith id m t2
                  where [t1,t2] = ts
     "List"  ->  "[" ++ showWith id m t1 ++ "]"
                  where [t1] = ts
     -- "Pair"  ->  "(" ++ showWith id m t1 ++ "," ++ showWith id m t2 ++ ")"
     --             where [t1,t2] = ts 
     "::"     ->  showWith id m t1 ++ " :: " ++ showWith id m t2  ++ "\n"
                  where [t1,t2] = ts            
     _        ->  if ts==[] then tcn 
                  else tcn ++ " " ++ (concat $ intersperse " " (map (showWith brack m) ts))
     

        
brack :: String -> String
brack s = "("++s++")"

varMap :: Type_exp -> [([Int],String)]
varMap t = zip (nub $ varsOf t) varNames
  where
  varNames = map (:[]) lett ++ concatMap (\n -> [c : show n | c <- lett]) [1..]
  lett = ['a'..'z']

varsOf :: Type_exp -> [[Int]]
varsOf (TVAR tvn) = [tvn]
varsOf (TCONS tcn ts) = concatMap varsOf ts 

--}
showfuntypes (fts,decls) =  (map showfuntype fts , decls )
         
showfuntype (f,t) =  (f,t)--f ++ " :: " ++ (show t) ++ "\n"

prettyBlock :: (a -> Doc) -> [a] -> Doc
prettyBlock f = braces . enclose line line . vsep . punctuate semi . map f

prettyAlt :: Alt -> Doc
prettyAlt (p, x) = nest 2 $ pretty p <+> text "->" </> pretty x

prettyBind :: Binding -> Doc
prettyBind (v, x) = text v <+> text "=" <+> pretty x

prettyArg :: Exp -> Doc
prettyArg (App e []) = prettyArg e
prettyArg e@(App _ _) = parens . pretty $ e
prettyArg e@(Case _ _) = parens . pretty $ e
prettyArg e@(Let _ _) = parens . pretty $ e
prettyArg e@(Lam _ _) = parens . pretty $ e
prettyArg e = pretty e

prettyArg2 :: Exp -> Doc
prettyArg2 (App e []) = prettyArg e
prettyArg2 e@(Case _ _) = parens . pretty $ e
prettyArg2 e@(Let _ _) = parens . pretty $ e
prettyArg2 e@(Lam _ _) = parens . pretty $ e
prettyArg2 e = pretty e

showPretty :: Pretty a => a -> String
showPretty = (++ "\n") . show  . indent 2 . pretty

{-
instance Show Decl where
	show = ('\n':) . show  . pretty
	
instance Show Exp where
	show = ('\n':) . show . pretty
-}
