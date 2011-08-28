{

init (Cons x Nil) = Nil;
init (Cons x (Cons y ys)) = Cons x (init (Cons y ys));

inits xs = case xs of {
             Nil -> Cons Nil Nil;
             Cons y ys -> Cons xs (inits (init xs));
           };

tails Nil = Nil;
tails (Cons x xs) = Cons (Cons x xs) (tails xs);

mapSum Nil = Nil;
mapSum (Cons x xs) = Cons (sum x) (mapSum xs);

append Nil ys = ys;
append (Cons x xs) ys = Cons x (append xs ys);

concatMapTails Nil = Nil;
concatMapTails (Cons x xs) = append (tails x) (concatMapTails xs);

segments xs = concatMapTails (inits xs);

maximum (Cons x xs) = max x xs;

max m Nil = m;
max m (Cons x xs) = case (<=) m x of { True -> max x xs ; False -> max m xs };

sum Nil = 0;
sum (Cons x xs) = (+) x (sum xs);

mss xs = maximum (mapSum (segments xs));

fromTo n m = case (<=) n m of {
               True -> Cons n (fromTo ((+) n 1) m);
               False -> Nil;
             };

main = mss (fromTo ((-) 0 150) 150);

}
