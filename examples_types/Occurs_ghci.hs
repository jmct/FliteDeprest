--data List a  = Nil | Cons a (List a); 
--f x = Cons x x;
fix1 f = f (fix1 f);
cons x y= x;
main = fix1 (9);


