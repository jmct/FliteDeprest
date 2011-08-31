module Flite.Defunct where

import Flite.Syntax
import Flite.Traversals
import Flite.Pretty
import Flite.ConcatApp
import Flite.Descend
import Flite.Fresh

import Debug.Trace
import List


type Request = (Id, Exp)

type Replacement = (Exp, Exp) -- (from, to)

defunctionalise :: Prog -> Fresh Prog
defunctionalise p = trace ("\n\n::::: After :::::::\n" ++ show p' ++ "\n::::::::::::\n" ) p'
    where
        p' = concatApps $ findUsedDecls $ defunctionalise' $
            trace ("::::: Before ::::::\n"   ++ show p ++ "\n::::::::::::\n" ) p

defunctionalise' :: Prog -> Prog
defunctionalise' p = after
    where
        before = p
        after  = case rs of 
            [] -> p'
            _  -> -- trace ("Requests:" ++ show rs)
                defunctionalise' $ makeRequestedDecls p' rs
        (p', rss) = unzip $ map (defuncRhs p) $ before
        rs = concat rss


-- we pass in Prog because we're going to need to know arity later.
defuncRhs :: Prog -> Decl -> (Decl, [Request])
defuncRhs p (Func id pats rhs) = (Func id pats rhs', rs)
    where 
        (rhs', rs) = 
            -- trace ("Stripping Funs passed as args in " ++ id) $
            defuncExp p rhs


defuncExp :: Prog -> Exp -> (Exp, [Request])
defuncExp p e@(App f args) = (e', rs)
    where
        e' = App f' args'
        (f', args', rs) = defuncApp p f args
defuncExp p e@(Case ex alts) = (Case ex' alts', rs')
    where
        (alts', rss) = unzip $ map (defuncAlt p) alts
        (ex', rs) = defuncExp p ex
        rs' = concat (rs:rss)
defuncExp p e@(Let bs ex) = (Let bs' ex', rs')
    where
        (bs', rss) = unzip $ map (defuncBinding p) bs
        (ex', rs) = defuncExp p ex
        rs' = concat (rs:rss)
defuncExp p e@(Lam is ex) = (Lam is ex', rs)
    where
        (ex', rs) = defuncExp p ex
defuncExp p e = 
--    trace ("Unchanged " ++ show e) 
    (e, [])


defuncBinding :: Prog -> Binding -> (Binding, [Request])
defuncBinding p (id, ex) = ( (id, ex'), rs )
    where
        ( ex', rs ) = defuncExp p ex

defuncAlt :: Prog -> Alt -> (Alt, [Request])
defuncAlt p (pat, ex) = ( (pat', ex'), rs'')
    where
        (pat', rs) = defuncExp p pat
        (ex', rs') = defuncExp p ex
        rs'' = rs ++ rs'

data AppClassification = 
    Primitive |
    Nullary |
    Partial |
    Saturated |
    OverApplied |
    HigherOrder |
    HigherOrderWithPartial |
    HigherOrderWithPartialPrimitive |
    Unclassified

classifyApplication :: Prog -> Exp -> AppClassification
classifyApplication p e@( App f args ) =
    case f of 
        Fun id -> classify id
        -- Con id -> classify id
        _ ->  Unclassified
    where 
        classify id =
            if isPrimId id then
                if isFunctionAppliedToPartial p e then
                    -- trace ("PartialPrimitive " ++ id)
                    HigherOrderWithPartialPrimitive
                else
                    -- trace ("Primitive " ++ id)
                    Primitive
            else if arityOf p id == 0 then
                -- trace ("Nullary " ++ id)
                Nullary
            else if arityOf p id > length args then
                -- trace ("Partial " ++ id)
                Partial
            else if arityOf p id < length args then
                -- trace ("OverApplied " ++ id)
                OverApplied
            else if isFunctionAppliedToPartial p e then
                -- trace ("HigherOrderWithPartial " ++ id)
                HigherOrderWithPartial
            else if (or $ map (isItAFunction p) args) then
                -- trace ("HigherOrder " ++ id)
                HigherOrder
            else
                -- trace ("Saturated " ++ id)
                Saturated
classifyApplication _ e = error $ "classifyApplication called on an expression other than an application: " ++ show e


defuncApp :: Prog -> Exp -> [Exp] -> (Exp, [Exp], [Request]) -- (f', args', rs)
defuncApp p f args =
    case classifyApplication p (App f' args') of
        HigherOrderWithPartial ->
            -- trace ("HOWP: " ++ show e) $
            defuncAppWithPartiallyAppliedArg p f' args' rs'
        HigherOrderWithPartialPrimitive -> 
            -- trace ("HOWPP: " ++ show e) $
            defuncAppWithPartiallyAppliedArg p f' args' rs'
        HigherOrder -> 
            -- trace ("HO: " ++ show e) $
            defuncApp' p f' args' rs'
        _ ->
            -- trace ("Other: " ++ show e) $
            (f', args', rs')
    where
        e = App f' args'
        (f', rs) = defuncExp p f
        (args', rss) = unzip $ map (defuncExp p) args
        rs' = concat (rs:rss)


-- The business-end of the basic defunctionaliser
defuncApp' :: Prog -> Exp -> [Exp] -> [Request] -> (Exp, [Exp], [Request])
defuncApp' p f args rs =
    -- trace ("defuncApp' Requesting " ++ sName) $
    (f', vArgs, rs')
    where 
        req = ( sName, App f args )
        rs' = req:rs
        sName = specialisedName p (f:fArgs)
        (fArgs, vArgs) = partitionArgs p args
        f' = case f of 
            Fun _ -> Fun sName
            Con _ -> Con sName
            _     -> error "This shouldn't be possible!"


isFunctionAppliedToPartial :: Prog -> Exp -> Bool
isFunctionAppliedToPartial p e@( App (Fun _) ((App (Fun id) aargs):_) ) =
    if length aargs < arityOf p id then True else False
isFunctionAppliedToPartial p e@( App (Fun _) ((App (Con id) aargs):_) ) = 
    if length aargs < arityOf p id then True else False
{-isFunctionAppliedToPartial p e@( App (Con _) ((App (Fun id) aargs):_) ) =
    if length aargs < arityOf p id then True else False
isFunctionAppliedToPartial p e@( App (Con _) ((App (Con id) aargs):_) ) = 
    if length aargs < arityOf p id then True else False -}
isFunctionAppliedToPartial p e =
    False


-- Defunctionalise a function applied to a partial application of another function. Only handles a contiguous list of partial apps at the start of the arg list. Don't know how much of a problem this will be.
defuncAppWithPartiallyAppliedArg :: Prog -> Exp -> [Exp] -> [Request] -> (Exp, [Exp], [Request])
defuncAppWithPartiallyAppliedArg p f args@((App af aargs):as) rs =
    -- trace ("defuncAppWithPartiallyAppliedArg Requesting " ++ newName ++ "\n" ++ show args') $
    case arityOf p (getId af) > 0 of
        True -> (f', args', rs')
        False -> (f, args, rs)
    where
        args' = aargs ++ as
        newName = specialisedName p (f:af:args')
        f' = Fun newName
        rs' = (newName, (App f args)):rs

partitionArgs :: Prog -> [Exp] -> ([Exp], [Exp]) -- (funcArgs, valArgs)
partitionArgs p = partition (isItAFunction p)


isItAFunction :: Prog -> Exp -> Bool
isItAFunction p (Fun id) = 
    -- trace (id ++ ": " ++ show isIt)
    isIt
    where
        isIt = if (arityOf p id) == 0 then False else True
isItAFunction p (Con id) =
    -- trace (id ++ ": " ++ show isIt)
    isIt
    where
        isIt = if (arityOf p id) == 0 then False else True
{-isItAFunction p (App (Fun id) args) =
    if (arityOf p id) > length args
        then True
        else False -}
isItAFunction p _ = False

specialisedName :: Prog -> [Exp] -> Id
specialisedName p es = -- trace ("Requested name for " ++ show es) $
    foldr ( (++) . ('^':) ) "" $ map specGetId es
    where
        specGetId (Fun id) = if arityOf p id == 0 then "" else id
        specGetId (Con id) = if arityOf p id == 0 then "" else id
        specGetId (App (Fun id) args) = show (length args) ++ id
        specGetId (App (Con id) args) = show (length args) ++ id
        specGetId _ = ""



getId :: Exp -> Id
getId (Fun id) = id
getId (Con id) = id
getId (App (Fun id) args) = id
getId (App (Con id) args) = id
getId e = "" -- error $ "Tried to getId of a non-function: " ++ show e




makeRequestedDecls :: Prog -> [Request] -> Prog
makeRequestedDecls p [] = p
makeRequestedDecls p (r:rs) = makeRequestedDecls p' rs
    where
        p' = (makeDeclFromRequest p r) ++ p


-- makeDeclFromRequest and specialiseDecl should only return a single
-- Decl, but they do so in a list to gracefully handle the possiblity
-- of a new Decl not being required.
makeDeclFromRequest :: Prog -> Request -> [Decl]
makeDeclFromRequest pr (newName, ex@(App f args)) =
    -- trace (show ex) $
    case (existing, classifyApplication pr ex) of
        ([], HigherOrderWithPartial) ->
            specialisePartiallyAppliedArg pr ex newName
        ([], HigherOrderWithPartialPrimitive) ->
            specialisePartiallyAppliedArg pr ex newName
        ([], HigherOrder) -> 
            specialiseDecl pr specialiseMe args newName
        (_, _) -> []
    where
        existing = getDeclFor pr newName
        specialiseMe = getDeclFor pr (getId f)
makeDeclFromRequest pr (newName, e@(Fun id)) = [Func newName args rhs]
    where
        args = case arityOf pr id of 
            1 -> [Var "?a"]
            2 -> [Var "?a", Var "?b"]
        rhs = (App e args)
makeDeclFromRequest pr e = error ("Non-application passed to function specialiser: " ++ show e)

arityOf :: Prog -> Id -> Int
arityOf p id =
    case ds of  -- needs special cases for all built-in constructors
        [] -> if isBinaryPrim id || id == "Cons" then 2
                else if isUnaryPrim id then 1
                else if id == "Nil" || id == "True" || id == "False" then 0
                else error ("Couldn't find a declaration for " ++ id ++ "\n\n...in..." ++ show p)
        [d] -> length $ funcArgs d
        _ -> error ("Multiple declarations for " ++ id)
    where
        ds = getDeclFor p id


-- specialiseDecl takes and returns lists of Decls to handle the 
-- possibility of a new Decl not being needed (ie, we simply 
-- return the empty list.
specialiseDecl :: Prog -> [Decl] -> [Exp] -> Id -> [Decl]
specialiseDecl p [d@(Func id params rhs)] args newId =
    -- trace ("Specialising " ++ id ++ " to " ++ newId) $
    [Func newId params' rhs']
    where
        whichArgsAreFunctional = map (isItAFunction p) args
        replaceMe = zip whichArgsAreFunctional params
        replaceWith = zip whichArgsAreFunctional args
        repls = zip (extractSndWhereFstIsTrue replaceMe) (extractSndWhereFstIsTrue replaceWith)
        rhs' = replaceInExp repls rhs
        params' = extractSndWhereFstIsFalse $ zip whichArgsAreFunctional params 
specialiseDecl p d as newId = error $ "Couldn't honour request for " ++ newId ++ "\n\n" ++ show (p, d, as)


-- Remove the first arg from d, and prepend the first n args from the decl for head aargs
specialisePartiallyAppliedArg :: Prog -> Exp -> Id -> [Decl]
specialisePartiallyAppliedArg p app@(App f1 (arg@(App f2 args2):as)) newId = 
    [Func newId params rhs]
    where
        d1 = head $ getDeclFor p (getId f1)
        d2 = head $ getDeclFor p (getId f2)
        params = (take (length args2) $ funcArgs d2) ++ (tail $ funcArgs d1)
        replaceMe = (head $ funcArgs d1, arg')
        arg' = App f2 (take (length args2) $ funcArgs d2)
        rhs = replaceInExp [replaceMe] $ funcRhs d1

{-

specialisePartiallyAppliedArg :: Prog -> [Decl] -> [Exp] -> Id -> [Decl]
specialisePartiallyAppliedArg p [d@(Func id (arg:args) rhs)] aargs newId =
    [Func newId args' rhs']
    where
        aparams = case head aargs of 
            (App (Fun aid) aas) -> take (length aas) $ funcArgs $ head $ getDeclFor p aid
            (App (Con aid) aas) -> take (length aas) $ funcArgs $ head $ getDeclFor p aid
            _ -> error $ show d
        args' = aparams ++ args
        replaceMe = (arg, head aargs)
        rhs' = replaceInExp [replaceMe] rhs 

combineArgsFromDecls :: Prog -> Decl -> Decl -> Exp -> Id -> [Decl]
combineArgsFromDecls p d1 d2 app newId =
    [Func newId params' rhs']
    where
        
-}

extractSndWhereFstIsTrue :: [(Bool, b)] -> [b]
extractSndWhereFstIsTrue = (map snd) . (filter fst)

extractSndWhereFstIsFalse :: [(Bool, b)] -> [b]
extractSndWhereFstIsFalse = (map snd) . (filter (not.fst))



replaceInExp :: [Replacement] -> Exp -> Exp
replaceInExp rs e = 
    if (length matches) > 0 then
        snd $ head matches
    else case e of
        (App f args) -> 
            App (replaceInExp rs f) (map (replaceInExp rs) args)
        (Case e alts) ->
            Case (replaceInExp rs e) (map (replaceInAlt rs) alts)
        (Let bs e) ->
            Let (map (replaceInLet rs) bs) (replaceInExp rs e)
        (Lam is e) ->
            Lam is (replaceInExp rs e)
        e -> e
    where
        matches = filter ( (==e) . fst ) rs

replaceInAlt :: [Replacement] -> Alt -> Alt
replaceInAlt rs a = (replaceInExp rs $ fst a, replaceInExp rs $ snd a)

replaceInLet :: [Replacement] -> Binding -> Binding
replaceInLet rs b = (fst b, replaceInExp rs $ snd b)


-- A wrapper around lookupFuncs that creates fake declarations for 
--   primitives.
getDeclFor :: Prog -> Id -> [Decl]
getDeclFor p id 
    | isBinaryPrim id || id == "Cons" =
        [ Func id [Var "?a", Var "?b"] (App (Fun id) [Var "?a", Var "?b"]) ]
    | isUnaryPrim id = 
        [ Func id [Var "?a"] (App (Fun id) [Var "?a"]) ]
    | otherwise   = lookupFuncs id p

-- And this does the same for constructors, by finding a pattern
--   match that uses them.
fakeDeclForCon :: Prog -> Id -> [Decl]
fakeDeclForCon p cid = fromExp findCon
    where
        findCon (Case ex alts)
            | null defs = concatMap findCon [ex:alts]
            | otherwise = case head defs of a@(App c args) -> makeFakeDecl a
                where
                    findCon e = descend findCon e
                    defs = filter ((isAnAppOfCon cid) . fst) alts
                    isAnAppOfCon cid (App (Con id) _) | cid == id = True
                    isAnAppOfCon _ _ = False


makeFakeDecl :: Exp -> [Decl]
makeFakeDecl a@(App c@(Con id) args) = 
    [Func id args (App c args)]


-- Tidy up the resulting program

findUsedDecls :: Prog -> Prog
findUsedDecls p = findUsedDecls' p $ getDeclFor p "main"


findUsedDecls' :: Prog -> [Decl] -> Prog
findUsedDecls' p acc =
    if length acc' == length acc 
        then acc'
        else findUsedDecls' p acc'
    where
        acc' = filter ( \d -> (funcName d) `elem` ("main":usedFuncs) ) p
        usedFuncs = catalogueCalledFunctions acc


catalogueCalledFunctions :: [Decl] -> [Id]
catalogueCalledFunctions ds = concatMap (findFunsInExp . funcRhs) ds


findFunsInExp :: Exp -> [Id]
findFunsInExp (App f args) = concatMap findFunsInExp (f:args)
findFunsInExp (Case e alts) =
    concatMap findFunsInExp (e:( (fst $ unzip alts) ++ (snd $ unzip alts)))
findFunsInExp (Let bs e) = concatMap findFunsInExp ( e:(snd $ unzip bs) )
findFunsInExp (Lam is e) = findFunsInExp e
findFunsInExp (Fun id) = [id]
-- findFunsInExp (Con id) = [id] 
findFunsInExp _ = []


