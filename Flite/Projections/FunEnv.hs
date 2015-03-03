module Flite.Projections.FunEnv where
import Flite.PrettyLib
import Flite.Projections.Contexts
import Data.Map.Strict as M
import Data.List (groupBy)
 {- Remember:
  - type FunEnv = M.Map (String, Context) Context
  -}

prettyFE = format . M.toList 

eqSep :: Doc
eqSep = string $ take 60 $ repeat '='


format :: [((String, Context), Context)] -> Doc
format xs = vsep $ fmap formatFunc $ groupBy (\((x, _), _) ((y, _), _) -> x == y) xs

formatFunc :: [((String, Context), Context)] -> Doc
formatFunc xs = enclose line line $ eqSep <$$> (string "== " <> name) <$$> eqSep <$$> trans
  where
    name  = string $ fst . fst $ head xs -- safe because fomatFunc is only ever 'mapped'
    trans = vsep $ fmap renderTrans xs

renderTrans :: ((String, Context), Context) -> Doc
renderTrans ((_, i), o) = input <$$> output
  where
    input  = string $ show i
    output = indent 8 $ string "==> " <> (string $ show o)
