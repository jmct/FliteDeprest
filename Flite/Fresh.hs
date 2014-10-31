module Flite.Fresh where

import Control.Monad (replicateM)

data Fresh a = Fresh { runFresh :: String -> Int -> (Int, a) }

instance Monad Fresh where
  return a = Fresh (\s i -> (i, a))
  m >>= f  = Fresh (\s i -> case runFresh m s i of
                              (j, a) -> runFresh (f a) s j)

fresh :: Fresh String
fresh = Fresh (\s i -> (i+1, s ++ show i))

freshList :: Int -> Fresh [String]
freshList n = replicateM n fresh
