module Flite.Projections.Contexts where

import Flite.Projections.Conversion
import Data.Generics.Uniplate.Direct
import Data.Generics.Str
import Data.Maybe (fromMaybe)



data Context = CVar String
             | CRec String
             | CBot
             | CProd [Context]
             | CSum [(String, Context)]
             | CMu String Context
             | CStr Context
             | CLaz Context
         deriving (Show, Eq)

-- Because we're using an Assc-List, it's useful to map over the range
mapRange :: (a -> b) -> [(c,a)] -> [(c,b)]
mapRange f xs = zip (map fst xs) $ map (f . snd) xs

zipWRange :: (a -> b -> c) -> [(d,a)] -> [(e,b)] -> [(d,c)]
zipWRange f xs ys = zip (map fst xs) $ zipWith f xs' ys'
    where xs' = map snd xs
          ys' = map snd ys

instance Uniplate Context where
    uniplate (CProd cs) = plate CProd ||* cs
    uniplate (CSum cs)  = (listStr (map snd cs), \str -> CSum (zip (map fst cs) (strList str)))
    uniplate (CMu n c)  = plate CMu |- n |* c
    uniplate (CStr c)   = plate CStr |* c
    uniplate (CLaz c)   = plate CLaz |* c
    uniplate c          = (Zero, \Zero -> c)

-- Conversion from our representation of types, into our 'prototype' context for the type.
-- The prototype context generated should be the 'Top' context, approximating Ide
fromPTExp :: PTExp -> Context
fromPTExp PTEmpty     = CProd []
fromPTExp (PTVar n)   = CVar n
fromPTExp (PTCon n e) = fromPTExp e
fromPTExp (PTSum es)  = CSum $ map f es
    where f (PTCon n e) = (n, fromPTExp e)
fromPTExp (PTProd es) = CProd $ map fromPTExp es
fromPTExp (Mu n e)    = muify $ CMu n $ fromPTExp e
fromPTExp (LiftT e)   = CLaz $ fromPTExp e

-- Because we need to treat u-bound variables differently, let's give them an
-- explicit representation
muify :: Context -> Context
muify (CMu n c) = CMu n $ transform f c
    where f (CVar n') | n' == n = CRec n
                      | otherwise = CVar n'
          f x = x
muify c         = c
 

-- Closer to working
allContexts :: Context -> [Context]
allContexts = concatMap allPrimContexts . concatMap allLiftContexts . allVarContexts

allPrimContexts :: Context -> [Context]
allPrimContexts c = concat . take n . iterate (concatMap primContexts) $ [c]
    where n = 1 + length [() | (CProd []) <- universe c]
primContexts :: Context -> [Context]
primContexts c = [ f j | (CProd [], f) <- contexts c, j <- [CBot]]

allLiftContexts :: Context -> [Context]
allLiftContexts c = concat . take n . iterate (concatMap liftContexts) $ [c]
    where n = 1 + length [() | (CLaz e) <- universe c]

liftContexts :: Context -> [Context]
liftContexts c = [ f j | (CLaz e, f) <- contexts c, j <- [CStr e]]

allVarContexts :: Context -> [Context]
allVarContexts c = concat . take n . iterate (concatMap varContexts) $ [c]
    where n = 1 + length [() | (CVar n) <- universe c]

varContexts :: Context -> [Context]
varContexts c = [ f j | (CVar n, f) <- contexts c, j <- [CBot]]

type ImEnv = [(String, Bool)]

-- Identify whether a given context is the bottom context for its type.
improper :: ImEnv -> Context -> Bool
improper e CBot       = True
improper e (CVar n)   = False
improper e (CProd []) = False
improper e (CRec n)   = fromMaybe False $ lookup n e
improper e (CLaz c)   = False && improper e c
improper e (CStr c)   = True && improper e c
improper e (CSum cs)  = and $ map (improper e . snd) cs
improper e (CProd cs) = or $ map (improper e) cs
improper e (CMu n c)  = improper ((n, True):e) c

-- Given a context, return the 'Bot' for that 'type'
-- 'Bad' version (N stands for naive)
mkBotN :: Context -> Context
mkBotN (CVar _)   = CBot
mkBotN (CRec n)   = CRec n
mkBotN CBot       = CBot
mkBotN (CProd []) = CBot
mkBotN (CLaz c)   = CStr $ mkBotN c
mkBotN (CStr c)   = CStr $ mkBotN c
mkBotN (CSum cs)  = CSum $ mapRange mkBotN cs
mkBotN (CProd cs) = CProd $ map mkBotN cs
mkBotN (CMu n c)  = CMu n $ mkBotN c

-- Given a context, return the 'Bot' for that 'type'
-- 'Good' version
mkBot :: Context -> Context
mkBot = transform f
    where f (CVar _)   = CBot
          f (CProd []) = CBot
          f (CLaz c)   = CStr c
          f c          = c

-- Given a context, return the context in normal form.
norm :: Context -> Context
norm (CLaz c)    = CLaz $ norm c
norm (CStr c)    = CStr $ norm c
norm (CSum cs)   = CSum $ mapRange norm cs
norm p@(CProd cs)
    | improper [] p = mkBot p
    | otherwise  = CProd $ map norm cs
norm r@(CMu n c)
    | improper [] r = mkBot r
    | otherwise  = CMu n $ norm c
norm c        = c

(\/) :: Context -> Context -> Context
c \/ CBot                = c
CBot \/ c                = c
(CProd []) \/ c          = CProd []
c \/ (CProd [])          = CProd []
(CSum cs) \/ (CSum ds)   = if fcs /= fds
                           then error "Performing disjunction on non-equivalent Contexts"
                           else CSum (zipWRange (\/) cs ds)
    where fcs = map fst cs
          fds = map fst ds
