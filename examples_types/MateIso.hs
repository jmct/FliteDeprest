{
data Kind       = King | Queen | Rook | Bishop | Knight | Pawn ;
data Colour     = Black | White ;
--type Piece Color Kind     =  Pair Colour Kind ;
--type Square Int Int       =  Pair Int Int ;
data Board      = Board (List (Pair Kind (Pair Int Int))) (List (Pair Kind (Pair Int Int))) ;
data Move       = Move (Pair Int Int) (Maybe (Pair Colour Kind) ) (Maybe (Pair Colour Kind)) ;
data MoveInFull = MoveInFull  (Pair Colour Kind) (Pair Int Int) Move ;
data Solution   = Solution MoveInFull (List (Pair MoveInFull Solution)) ;
--data Pair a b   = Pair a b;
data Maybe a    = Nothing | Just a;
data List a     = Nil | Cons a (List a);

con True  q = q ;
con False q = False ;

--fst (Pair x y) = x ;
--
--pieceAt (Board w b) =   pieceAtW (Pair w b);
  
--pieceAtW  Nil =   Nothing ;--True ;--Nothing ;--
--pieceAtW  Nil =   Nothing;   
--pieceAtW  (Cons (Pair k x) xs) =  Just k; --(Cons (Pair k s) xs) =  k;--(Cons (Pair k s) xs); --False; --Just k  ;  
             

pieceAt (Board wkss bkss) sq =
  pieceAtWith sq White (pieceAtWith sq Black Nothing bkss) wkss ;


pieceAtWith sq c n (Cons (Pair k s) xs) = 
case sameSquare s sq of {
  True -> Just (Pair c k) ;
  False -> pieceAtWith sq c n xs ;
  } ;
  

--p0 = Cons (Pair 0 0) (
--                       Cons (Pair 1 0) (
--                       Cons (Pair 2 0) (
--                       Cons (Pair 3 10000) (
--                       Cons (Pair 4 0) (
--                       Cons (Pair 5 0) Nil))))); 

--p1 (Nil)      = Nothing;
--p1 (Cons x xs)= Just x;

--p3= p1 p0;


--head (Cons x xs)  =  Just x ;

--tail (Cons x xs)  =  Just xs ;


length xs = case xs of {
                            Nil        -> 1;
                            Cons x xs' -> (+) True (length xs');
                            };
--(Pair k s)
--pieceAtWith sq c n (Cons (Pair k s) xs) = 
--  case (<=) 4 3  of {
--  True -> Just (Pair c k) ;
--  False -> Just (Pair c k); --pieceAtWith sq c n xs ;
--  } ;



sameSquare (Pair f1 r1) (Pair f2 r2) = con ((==) f1 f2) ((==) r1 r2) ;


--kindToChar k =
--	case k of {
--	King	  -> 'K' ;
--	Queen	  -> 'Q' ;
--	Rook	  -> 'R' ;
--	Bishop	  -> 'B' ;
--	Knight	  -> 'N' ;
--	Pawn	  -> 'P' ;
--  } ;

--isKing k = (==) (kindToChar k) 'K' ;


--problem =
--  Pair
--    ( Board
--      (Cons (Pair Knight (Pair 7 8))
--      (Cons (Pair Rook   (Pair 5 7))
--      (Cons (Pair King   (Pair 8 7))
--      (Cons (Pair Bishop (Pair 4 5))
--      (Cons (Pair Pawn   (Pair 8 4))
--      (Cons (Pair Pawn   (Pair 7 3))
--      (Cons (Pair Pawn   (Pair 5 2))
--      (Cons (Pair Pawn   (Pair 6 2))
--      (Cons (Pair Queen  (Pair 5 1))
--      Nil)))))))))
--      (Cons (Pair Knight (Pair 2 8))
--      (Cons (Pair Pawn   (Pair 7 7))
--      (Cons (Pair Pawn   (Pair 4 6))
--      (Cons (Pair Pawn   (Pair 3 5))
--      (Cons (Pair King   (Pair 6 5))
--      (Cons (Pair Pawn   (Pair 8 5))
--      (Cons (Pair Pawn   (Pair 4 4))
--      (Cons (Pair Pawn   (Pair 2 3))
--      (Cons (Pair Pawn   (Pair 5 3))
--      (Cons (Pair Pawn   (Pair 7 2))
--      (Cons (Pair Queen  (Pair 1 1))
--      (Cons (Pair Knight (Pair 2 1))
--      (Cons (Pair Bishop (Pair 8 1))
--      Nil)))))))))))))
--    )
--    (Pair White 3) ;




main = 1; --length p0  ; -- let {board = fst problem } in
       --     case (isKing (King) ) of -- pieceAt board (Pair 6 5))) of
       --     {
       --        True -> pieceAt board (Pair 6 5);
       --        False  -> pieceAt board (Pair 3 5);
       --      };


--solveProblem (Pair bd (Pair c n)) = solve bd c n ;

}
