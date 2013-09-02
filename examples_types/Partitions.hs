{
--Haskell Code
--parts  0 = 1
--parts  n = sum [ comb (n-1,j) * parts (n-j-1) | j <- [0..n-1]]
              
--comb (n, m) = fact n `div` (fact m * fact (n - m))

--fact 0 = 1
--fact n = n * fact (n-1)

-- My Flite attempt. 
-- The False case in parts function is not complete, Is there any efficient way to do that (list comprehension) in Flite?
-- So that part should be provided by every Reduceron club member. :)
-- I haven't run this example, it is just type-checked.
data List a  = Cons a (List a) | Nil; 

parts n  = case (<=) 0 n of {
                 True   ->  1; 
                 False  ->  1; --sum [(mul (comb ((-) n 1) j) (parts ((-) ((-) n j) 1))) <- [0..n-1]; How to do this in Flite?
                };

fact n = case (<=) 0 n of {
                 True   -> 1;
                 False  -> mul n (fact ((-) n 1));
                };

comb n m = case (<=) n m of {
                 True  -> 0;
                 False -> case divMod (fact n) (mul (fact m) (fact ((-) n m))) of {
                           Pair d m -> d;
                          } ;
                 };
            
sum Nil          = 0;
sum (Cons x xs)  = x + sum xs;


--Colin's contribution, to allow multiplication and division in Flite.
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
 
mul x n = case (==) n 1 of {
          True  -> x ;
          False -> case divMod n 2 of {
                    Pair d m  -> (+) (mul ((+) x x) d) 
                                 (case (==) m 0 of {True  -> 0; False -> x;}) ;
                                      } ;
          } ;

main =  parts 3  -- it should compute 5.
}
 
