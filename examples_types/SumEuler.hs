{

data Bool = True | False ;
data List a = Cons a (List a) | Nil ;

ifte p t f = case p of {
              True -> t;
              False -> f;
           };

gcd x y = ifte ((==) y 0) x (ifte ((>) x y) (gcd ((-) x y) y) (gcd x ((-) y x)));

fromto x y = ifte ((>) x y) Nil (Cons x (fromto ((+) x 1) y));

downfrom n = ifte ((==) n 0) Nil (Cons n (downfrom ((-) n 1)));


map f Nil = Nil;
map f (Cons x xs) = Cons (f x) (map f xs);


relPrime x y = (==) (gcd x y) 1;

filter p Nil = Nil;
filter p (Cons x xs) = ifte (p x) (Cons x (filter p xs)) (filter p xs);


length Nil = 0;
length (Cons x xs) = (+) 1 (length xs);

sum Nil = 0;
sum (Cons x xs) = (+) x (sum xs);

euler n = let { xs = fromto 1 n ; }
          in length (filter (relPrime n) xs);

main = sum (map euler (fromto 1 1000));

}
