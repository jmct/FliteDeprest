{
mul x n = case (==) n 1 of {
          True  -> x ;
          False -> case divMod n 2 of {
                    Pair d m  -> (+) (mul ((+) x x) d) 
                                 (case (==) m 0 of {True  -> 0; False -> x;}) ;
                                      } ;
          } ;

divMod x y = let { y2 = (+) y y } in
               case (<=) y2 x of {
               True  -> case divMod x y2 of {
                        Pair d2 m2 -> case (<=) y m2 of {
                                      True  -> Pair ((+) 1 ((+) d2 d2)) ((-) m2 y);
                                      False -> Pair ((+) d2 d2) m2 ;
                                      } ;
                        } ;
               False -> case (<=) y x of {
                        True  -> Pair 1 ((-) x y) ;
                        False -> Pair 0 x ;
                        } ;
               } ;


--min x y = case (<=) y x of {
--                        True  -> y ;
--                        False -> x ;
--                        } ;
--               } ;
 
main =  mul 4 5;
}
