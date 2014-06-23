{

data Bool    = True | False ;
data List a  = Cons a (List a) | Nil ;
data Triple a b c = A a | B b | C c | D a (Triple a b c);  

map f Nil = Nil;
map f (Cons x xs) = Cons (f x) (map f xs);

myFunc t = case t of {
            A a -> A a;
            B a -> B a;
            C a -> C a
           };

append Nil ys = ys;
append (Cons x xs) ys = Cons x (append xs ys);
}


