{
data List a  = Nil | Cons a (List a); 
--sum a b = case  (<=) a b of {
--          True   -> (+) a b;
--          False  -> rest a b;
--          };

--rest a b = case (<=) a b of {
--           True -> (-) a b;
--           False -> sum  b a;
--          };

--sumOftwo = sum 44 (rest 20 30);

f x = Cons x x;

main = f 1;
}
