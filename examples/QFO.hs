{

main = length (gen 10 10);

and False a = False;
and True a = a;

append Nil ys = ys;
append (Cons x xs) ys = Cons x (append xs ys);

length Nil = 0;
length (Cons x xs) = (+) 1 (length xs);

gen nq n =
  case (==) n 0 of {
    True -> Cons Nil Nil;
    False -> concatMapGen1 nq (gen nq ((-) n 1));
  };

concatMapGen1 nq Nil = Nil ;
concatMapGen1 nq (Cons x xs) = append (gen1 nq x) (concatMapGen1 nq xs) ;


gen1 nq b = concatMapGen2 b (toOne nq);

concatMapGen2 b Nil = Nil ;
concatMapGen2 b (Cons q qs) = append (gen2 b q) (concatMapGen2 b qs) ;

gen2 b q = case safe q 1 b of {
             True -> Cons (Cons q b) Nil;
             False -> Nil;
           };

safe x d Nil = True;
safe x d (Cons q l) =
  and ((/=) x q) (
  and ((/=) x ((+) q d)) (
  and ((/=) x ((-) q d)) (
  safe x ((+) d 1) l)));       

toOne n = case (==) n 1 of {
            True -> Cons 1 Nil;
            False -> Cons n (toOne ((-) n 1));
          };

}
