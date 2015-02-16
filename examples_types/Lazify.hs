{

data IntList = INil | ICons Int IntList;
data MyPair a b = MkPr a b;
data Bool    = True | False ;
data AbsPair = AP (MyPair Bool Bool);
data List a  = Cons a (List a) | Nil ;
data Triple a b c = A a | B b | C c | D a (Triple a b c);  
data Shrub = Root Node;
data Node = Void | Fork Shrub Int Shrub;
data Rose a = R a (List (Rose a));
data Tree a = Empty | BNode (Tree a) a (Tree a);
data TreeBool = Leaf Bool | TNode TreeBool Bool TreeBool;
data Unit = One;

map f Nil = Nil;
map f (Cons x xs) = Cons (f x) (map f xs);

superTrue x = True;

fst (MkPr x y) = let { z = x; } in z;

snd (MkPr x y) = y;

megaAdd y = let { z = MkPr 1 2; } in ifte True ((+) y y) ((+) (fst z) (snd z));

addFiveL xs = map ((+) 5) xs;

myFunc t = case t of {
            A a -> A a;
            B a -> B a;
            C a -> C a
           };

ifte p t e = case p of {
            True -> t;
            False -> e
           };

append Nil ys = ys;
append (Cons x xs) ys = Cons x (append xs ys);

main = case (superTrue 3) of {
        True -> append (Cons 1 Nil) (addFiveL (Cons 1 (Cons 2 (Cons 3 Nil))));
        False -> Cons (megaAdd 3) Nil
       };
}


