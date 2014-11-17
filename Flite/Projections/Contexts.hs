module Flite.Projections.Contexts where

import Flite.Projections.Conversion
import Data.Generics.Uniplate.Direct
import Data.Generics.Str
import Data.Maybe (fromMaybe)
import Data.List (nub, findIndex)
import Data.Set ( isSubsetOf
                , union
                , unions
                , elems
                , insert
                , singleton
                , empty
                , Set
                )
import qualified Data.Set as S



data Context = CVar String
             | CTVar String Context
             | CRec String
             | CBot
             | CProd [Context]
             | CSum [(String, Context)]
             | CMu String Context
             | CStr Context
             | CLaz Context
             | Context :&: Context
             | Context :+: Context
         deriving (Show, Eq, Ord)

data CDataDec = CData { cDataName :: String
                      , cDataArgs :: [String]
                      , cDataCont :: Context
                      }
         deriving (Show, Eq, Ord)

-- See if the contexted represents a 'lifted type'
isLifted :: Context -> Bool
isLifted (CLaz _) = True
isLifted (CStr _) = True
isLifted _        = False

getLift :: Context -> Context -> Context
getLift (CLaz _) = CLaz
getLift (CStr _) = CStr
getLift _        = error "Trying to use 'getLift' on non-lifted context"

blankContext :: Context -> Context
blankContext = transform f
  where
    f (CVar _)  = CVar ""
    f (CMu _ c) = CMu "" c
    f (CRec _)  = CRec ""
    f c         = c

lookupByName :: String -> [CDataDec] -> Maybe CDataDec
lookupByName s []     = Nothing
lookupByName s (c@(CData n _ _):xs)
    | n == s    = Just c
    | otherwise = lookupByName s xs

getBName :: Context -> String
getBName (CMu s _) = s
getBName _         = error $ "String to get bound name from a non-recursive context"

getCSumNames :: Context -> [String]
getCSumNames (CSum cs) = map fst cs
getCSumNames (CMu _ (CSum cs)) = map fst cs
getCSumNames c         = error $ "attempt to extract cNames from" ++ show c

infixr 2 :+:
infixr 3 :&:
infix 1 <~
infixr 2 \/
infixr 3 &


-- replaces the matching association in a list, if not found it's equivalent to id
replace :: Eq a => [(a, b)] -> (a , b) -> [(a, b)]
replace [] _ = []
replace (x:xs) (a, b)
    | fst x == a = (a, b) : xs
    | otherwise  = x : replace xs (a, b)


-- The prototypes for each data declaration
prototypes :: [PDataDec] -> [CDataDec]
prototypes ds = map f ds
    where f (PData n as e) = CData n as $ fromPTExp e

-- Grab the relevant context for a specific constructor in a Sum-type
out :: String -> Context -> Context
out n (CSum cs)         = case lookup n cs of
                    Just c  -> c
                    Nothing -> error $ "Trying to extract undefined constructor " ++ show n ++ "from " ++ show cs ++ "\n"
out n (CMu _ (CSum cs)) = case lookup n cs of
                    Just c  -> c
                    Nothing -> error $ "Trying to extract undefined constructor " ++ show n ++ "from " ++ show cs ++ "\n"
out n c  = error $ "This is the context: " ++ show c ++ " This is the Cons: " ++ show n

-- Grab the relevant context for a specific constructor in a Sum-type
out' :: [CDataDec] -> String -> Context -> Context
out' prots n (CSum cs)         = case lookup n cs of
                    Just c  -> c
                    Nothing -> error $ "Trying to extract undefined constructor " ++ show n ++ "from " ++ show cs ++ "\n"
out' prots n (CMu _ (CSum cs)) = case lookup n cs of
                    Just c  -> c
                    Nothing -> error $ "Trying to extract undefined constructor " ++ show n ++ "from " ++ show cs ++ "\n"
out' prots n CBot  = out' prots n c
  where
    c = mkBot $ cDataCont $ foundIn n prots
out' prots n c  = error $ "This is the context: " ++ show c ++ " This is the Cons: " ++ show n

-- Insert context on a constructor into a context on the sum-type
inC :: String -> Context -> [CDataDec] -> Context
inC n c ds = case c' of
                []     -> error $ "Trying to insert an undefined constructor: " ++ show n
                (x:xs) -> x
    where c' = [CSum $ replace cs (n, c) | (CSum cs) <- universe b, n `elem` (map fst cs)]
          b  = mkBot $ cDataCont $ foundIn n ds


outProd :: Int -> Context -> Context
outProd n (CProd as) = as !! n
outProd _ _          = error $ "You cannot call \"outProd\" on non-CProducts"

-- Because this function is only used in foldUp we don't have to be as general
-- in fact, we can't be as general because it's possible to have a recursive
-- context as part of a non-recursive one. Like a list argument to a pair
isRec :: Context -> [CDataDec] -> Bool
isRec (CMu _ _) _  = True
isRec (CSum cs) ds = isMu dec
  where
    (CData _ _ dec) = foundIn (fst $ head cs) ds
    isMu (CMu _ _)  = True
    isMu _          = False
    

-- Find the prototype for the type that includes the given Constructor name
foundIn :: String -> [CDataDec] -> CDataDec
foundIn n ds = case pos of
                   Just i  -> ds !! i --guaranteed to be in bounds since findIndex works on list of same length
                   Nothing -> error "Trying to insert a constructor into its sum-type when constructor was never defined"
    where cons d = concat [cs | (CSum cs) <- universe d]
          pos    = findIndex id $ map (any ((== n) . fst) . cons . cDataCont) ds

-- This is only for when we are representing a context in the form a :+: b,
-- which is not always the case
toSet :: Context -> Set (Set Context)
toSet CBot      = empty
toSet (c :+: d) = toSet c `union` toSet d
toSet c         = singleton $ toSet' c

toSet' :: Context -> Set Context
toSet' CBot      = empty
toSet' (c :&: d) = unions $ (elems $ toSet c) ++ (elems $ toSet d)
toSet' c         = singleton c

filterEOut :: Set (Set a) -> Set (Set a)
filterEOut =  S.filter (not . S.null)


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
    uniplate (CTVar n c) = plate CTVar |- n |* c
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
 
-- return all of the _principle_ contexts from a prototype
allPrinContexts :: Context -> [Context]
allPrinContexts = nub . map norm . concatMap allPrimContexts . allLiftContexts

-- return all variations of a context. The resulting list could have multiple
-- variations of an equivalent context. 
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
improper e (CTVar a c) = c == CBot
improper e (CProd []) = False
improper e (CRec n)   = fromMaybe False $ lookup n e
improper e (CLaz c)   = False && improper e c
improper e (CStr c)   = True && improper e c
improper e (CSum cs)  = and $ map (improper e . snd) cs
improper e (CProd cs) = or $ map (improper e) cs
improper e (CMu n c)  = improper ((n, True):e) c
improper e c          = error $ "What? \n\n" ++ show c

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
mkBotN c          = c

-- Given a context, return the 'Bot' for that 'type'
-- 'Good' version
mkBot :: Context -> Context
mkBot = transform f
    where f (CVar _)   = CBot
          f (CProd []) = CBot
          f (CLaz c)   = CStr c
          f c          = c

-- Given a conext, return the 'Abs' for that type
mkAbs :: Context -> Context
mkAbs = transform f
    where f (CLaz c)   = CLaz (mkBot c)
          f (CStr c)   = CLaz (mkBot c)
          f (CSum cs)  = CSum $ map mkAbsCs cs
          f c          = c
{-
mkAbs (CLaz c)   = CLaz (mkBot c)
mkAbs (CSum cs)  = CSum $ map mkAbsCs cs
mkAbs (CProd cs) = CProd $ map mkAbs cs
-}

mkAbsCs :: (String, Context) -> (String, Context)
mkAbsCs (n, CBot) = (n, CProd [])
mkAbsCs (n, c)    = (n, mkAbs c)

hasESeq :: Context -> Bool
hasESeq (CSum cs) = or $ map isECon cs

isECon :: (String, Context) -> Bool
isECon ((n, CBot))     = True
isECon ((n, CProd [])) = True
isECon _               = False

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

(<<=) :: Context -> Context -> Bool
x <<= y = (x \/ y) == y

(#>) :: Context -> Context -> Context
(CLaz _) #> k = k \/ mkAbs k
(CStr _) #> k = k

-- Disjunction of demands. A value is only acceptable to the demand (a \/ b) if
-- it is acceptable to a _or_ to b
-- This definition is a translation of definition 6.21 in Hinze's diss.
-- infixr 2 \/
(\/) :: Context -> Context -> Context
(CTVar a c) \/ (CTVar b d) = CTVar a (c :+: d)
(CVar a) \/ (CVar b)     = CVar a
(CRec a) \/ (CRec b)     = CRec a
c \/ CBot                = c
CBot \/ c                = c
(CProd []) \/ c          = CProd []
c \/ (CProd [])          = CProd []
(CSum cs) \/ (CSum ds)   = if fcs /= fds
                           then error "Performing disjunction on non-equivalent Sum-Contexts"
                           else CSum (zipWRange (\/) cs ds)
    where fcs = map fst cs
          fds = map fst ds
(CProd cs) \/ (CProd ds) = if length cs /= length ds
                           then error "Performing disjunction on non-equivalent Prod-Contexts"
                           else CProd $ zipWith (\/) cs ds
(CLaz c) \/ (CLaz d)     = CLaz $ c \/ d
(CStr c) \/ (CLaz d)     = CLaz $ c \/ d
(CLaz c) \/ (CStr d)     = CLaz $ c \/ d
(CStr c) \/ (CStr d)     = CStr $ c \/ d
(CMu n c) \/ (CMu o d)   = CMu n $ reRec n $ c \/ d
x \/ y                   = x :+: y

reRec :: String -> Context -> Context
reRec n = transform f
  where f (CRec _) = CRec n
        f x        = x

joinList :: [Context] -> Context
joinList = foldr1 (\/)


-- Conjunction of demands. A value is only acceptable to the demand (a \/ b) if
-- it is acceptable to a _and_ to b
-- NOTE: I removed the call to 'norm' that is outlined in Hinze's thesis. 
-- I believe that this is safe for our purposes, but I am also not very bright.
-- TODO: Figure out if it _is_ safe. Prove it if possible.
-- infixr 3 &
(&) :: Context -> Context -> Context
(CTVar a c) & (CTVar b d) = CTVar a (c :&: d)
(CVar a) & (CVar b)     = CVar a
(CRec a) & (CRec b)     = CRec a
c & CBot                = CBot
CBot & c                = CBot
(CProd []) & (CProd []) = CProd []
(CLaz c) & (CLaz d)     = CLaz $ c \/ d
(CStr c) & (CLaz d)     = CStr $ c \/ c & d
(CLaz c) & (CStr d)     = CStr $ c & d \/ d
(CStr c) & (CStr d)     = CStr $ c & d
(CSum cs) & (CSum ds)   = if fcs /= fds
                          then error "Performing conjunction on non-equivalent Sum-Contexts"
                          else CSum (zipWRange (&) cs ds)
    where fcs = map fst cs
          fds = map fst ds
(CProd cs) & (CProd ds) = if length cs /= length ds
                          then error "Performing conjunction on non-equivalent Prod-Contexts"
                          else norm $ CProd $ zipWith (&) cs ds
(CMu n c) & (CMu o d)
    | k <~ y1 :&: y2        = norm $ CMu n $ c & d
    | k <~ y1 :+: y1 :&: y2 = CMu n $ reRec n $ c \/ c & d
    | k <~ y1 :&: y2 :+: y2 = CMu n $ reRec n $ c & d \/ d
    | k <~ top              = CMu n $ reRec n $ c \/ d
  where k = filterEOut $ toSet $ lubA "a" $ ((subRec (CTVar "a" (CVar "y1")) c)) & ((subRec (CTVar "a" (CVar "y2")) d))
        y1 = CVar "y1"
        y2 = CVar "y2"
        top = y1 :+: y1 :&: y2 :+: y2
x & y                   = x :&: y
--x & y = error $ "\n\nThe 'lubA' contexts found no match... This is very impossible\n\nx: " ++ show x ++ "\ny: " ++ show y


-- infix 1 <~
(<~) :: Set (Set Context) -> Context -> Bool
k <~ c = k `isSubsetOf` toSet c

subRec :: Context -> Context -> Context
subRec c = transform f
  where f (CRec _) = c
        f x        = x

lubA :: String -> Context -> Context
lubA a (CTVar b c)     = if a == b then c else CBot
lubA a (CStr (CVar b)) = CBot
lubA a (CLaz (CVar b)) = CBot
lubA a (CRec b)          = CRec b
lubA a (CProd [])        = CBot
lubA a CBot              = CBot
lubA a (CStr c)          = lubA a c
lubA a (CLaz c)          = lubA a c
lubA a (CSum cs)         = joinList $ map (lubA a . snd) cs
lubA a (CProd cs)        = joinList $ map (lubA a) cs
lubA a (CMu n c)         = lubA a c
