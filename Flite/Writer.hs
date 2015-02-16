module Flite.Writer where
import Control.Applicative

data Writer w a = W [w] a

instance Functor (Writer w) where
  fmap f (W w x) = W w (f x)

instance Applicative (Writer w) where
  pure a = W [] a
  W w f <*> W w' x = W (w ++ w') (f x)

instance Monad (Writer w) where
  return a = W [] a
  W w0 a0 >>= f = case f a0 of W w1 a1 -> W (w0 ++ w1) a1

runWriter :: Writer w a -> ([w], a)
runWriter (W ws a) = (ws, a)

write :: w -> Writer w ()
write w = W [w] ()

writeMany :: [w] -> Writer w ()
writeMany ws = W ws ()
