{

data IntList = INil | ICons Int IntList;
data MyPair a b = MkPr a b;
data Bool    = True | False ;
data AbsPair = AP (MyPair Bool Bool);
data List a  = Cons a (List a) | Nil ;
data Triple a b c = A a | B b | C c | D a (Triple a b c);  
data Shrub = Root Node;
data Node = Void | Fork Shrub Shrub;
data Rose a = R a (List (Rose a));

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


