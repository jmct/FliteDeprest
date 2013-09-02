{
data List a  = Nil | Cons a (List a);
---ength = lengthPlus 0;

--length Nil = 0 ;
--length (Cons x xs) = (+) 1 (length xs) ;


--map f Nil = Nil;
--map f (Cons x xs) = Cons (f x) (map f xs);


append Nil ys = ys;
append (Cons x xs) ys = Cons x (append xs ys);

--and False x = False;
--and True x = x;



--twice f x  =  f (f x);

--double x   = (+) x x;
--quad       = twice double;
main       =   1--quad 3 --and False True-- length  (Cons 1 Nil); 

}



