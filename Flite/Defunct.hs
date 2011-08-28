module Flite.Defunct where

import Flite.Syntax
import Flite.Traversals
import Flite.Descend

import Debug.Trace
import List



defunctionalise :: Prog -> Prog
defunctionalise p = p'
    where
        p' = p
        findCandidates :: Exp -> [Exp]
        findCandidates (App f args) 
            | or (map isItAFunction args) = [App f args] ++ (map findCandidates args)
        findCandidates e = descend findCandidates e

        --defuncExp :: Exp -> Exp


        isItAFunction :: Exp -> Bool
        isItAFunction (Fun id) = if (arityOf p id) == 0 then False else True
        isItAFunction (Con id) = True
        isItAFunction _ = False


        arityOf :: Id -> Int
        arityOf id =
            case ds of 
                [] -> if isBinaryPrim id then 2
                        else if isUnaryPrim id then 1
                        else error ("Couldn't find a declaration for " ++ id)
                [d] -> length $ funcArgs d
                _ -> error ("Multiple declarations for " ++ id)
            where
                ds = findDecl p id


        findDecl :: Id -> [Decl]
        findDecl id = filter ( (==id) . funcName  ) p




{-






type Request = (Id, Exp)

type Replacement = (Exp, Exp) -- (from, to)

defunctionalise :: Prog -> Prog
defunctionalise p = trace ("\n\n::::: After :::::::\n" ++ show p' ++ "\n::::::::::::\n" ) p'
    where
        p' = findUsedDecls $ defunctionalise' $
            trace ("::::: Before ::::::\n"   ++ show p) p

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


defuncApp :: Prog -> Exp -> [Exp] -> (Exp, [Exp], [Request]) -- (f', args', rs)
defuncApp p f args =
    case ( or $ map (isItAFunction p) args', f' ) of
        (True, Fun _) -> 
            if isPrimId id then
                -- Replace applications of primitive functions with equivalent decls
                --  so we can subsequently inline them.
                ( Fun fakeName, args', ((fakeName, Fun id):rs') )
            else if arity > length args' then -- Over-saturated app
                (f', args', rs')
            else if arity == 0 then -- top level var -- should be inlined maybe?
                (f', args', rs')
            else if arity < length args' then -- partial app
                (f', args', rs')
                -- saturate p f' args' rs'
            else -- The straightforward case...
                defuncApp' p f' args' rs'
            where
                fakeName = (specialisedName [f', Var "", Var ""])
        {-(_, App ff, fargs) -> -- an application (partial or otherwise) passed as a function.
            if farity == length fargs' then
                
            where
                farity = arityOf p id
                (ff', frs) = defuncExp p ff
                (fargs', frss) = unzip $ map (defuncExp p) fargs'
                frs' = concat (frs:frss) -}

        _ -> (f', args', rs')
    where
        arity = arityOf p id
        id = getId f'
        (f', rs) = defuncExp p f
        (args', rss) = unzip $ map (defuncExp p) args
        rs' = concat (rs:rss)


-- The business-end of the basic defunctionaliser
defuncApp' :: Prog -> Exp -> [Exp] -> [Request] -> (Exp, [Exp], [Request])
defuncApp' p f args rs = (f', vArgs, rs')
    where 
        req = ( sName, App f args )
        rs' = req:rs
        sName = specialisedName (f:fArgs)
        (fArgs, vArgs) = partitionArgs p args
        f' = case f of 
            Fun _ -> Fun $ sName
            Con _ -> Con $ sName
            _     -> error "This shouldn't be possible!"


partitionArgs :: Prog -> [Exp] -> ([Exp], [Exp]) -- (funcArgs, valArgs)
partitionArgs p = partition (isItAFunction p)


isItAFunction :: Prog -> Exp -> Bool
isItAFunction p (Fun id) = if (arityOf p id) == 0 then False else True
isItAFunction p (Con id) = True
isItAFunction p _ = False

specialisedName :: [Exp] -> Id
specialisedName es = -- trace ("Requested name for " ++ show es) $
    foldr ( (++) . ('^':) ) "" $ map getId es


getId :: Exp -> Id
getId (Fun id) = id
getId (Con id) = id
getId e = ""




makeRequestedDecls :: Prog -> [Request] -> Prog
makeRequestedDecls p [] = p
makeRequestedDecls p (r:rs) = makeRequestedDecls p' rs
    where
        p' = (makeDeclFromRequest p r) ++ p


-- makeDeclFromRequest findDecl and specialiseDecl only return a single
-- Decl, but they do so in a list to gracefully handle the possiblity
-- of a new Decl not being required.
makeDeclFromRequest :: Prog -> Request -> [Decl]
makeDeclFromRequest pr (newName, ex@(App f args)) =
    -- trace ("Handling request for " ++ newName ++ ": " ++ show ex) $
    case existing of
        [] -> d   -- need to create a Decl
            where
                d = specialiseDecl pr specialiseMe args newName
        ds -> []  -- declaration for this function already exists
    where
        existing = findDecl pr newName  
        specialiseMe = case f of
            ( Fun id ) -> findDecl pr id
            ( Con id ) -> findDecl pr id
            exp        -> error $ "Couldn't find a declaration for " ++ show exp
makeDeclFromRequest pr (newName, e@(Fun id)) = [Func newName args rhs]
    where
        args = case arityOf pr id of 
            1 -> [Var "?1"]
            2 -> [Var "?1", Var "?2"]
        rhs = (App e args)
makeDeclFromRequest pr e = error ("Non-application passed to function specialiser: " ++ show e)

arityOf :: Prog -> Id -> Int
arityOf p id =
    case ds of 
        [] -> if isBinaryPrim id then 2
                else if isUnaryPrim id then 1
                else error ("Couldn't find a declaration for " ++ id)
        [d] -> length $ funcArgs d
        _ -> error ("Multiple declarations for " ++ id)
    where
        ds = findDecl p id



findDecl :: Prog -> Id -> [Decl]
findDecl pr id = 
    -- trace ("Search: " ++ id ++ "\nFound: " ++ show found ) $
    found
    where
        found = filter ( (==id) . funcName  ) pr

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
specialiseDecl _ _ _ newId = error $ "Couldn't honour request for " ++ newId

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


-- Tidy up the resulting program

findUsedDecls :: Prog -> Prog
findUsedDecls p = findUsedDecls' p $ findDecl p "main"


findUsedDecls' :: Prog -> [Decl] -> Prog
findUsedDecls' p acc =
    if length acc' == length acc 
        then trace (show acc) acc'
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
-- findFunsInExp (Con id) = [id] -- top-level constructors get inlined with this commented. Good/bad? Not sure.
findFunsInExp _ = []

-}
