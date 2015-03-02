{

data Tree a = Empty | Node (Tree a) a (Tree a);
data Bool = True | False;

mkTree n = ifte ((==) n 0) Empty (Node (mkTree ((-) n 1)) n (mkTree ((-) n 1)));

ifte p t e = case p of {
            True -> t;
            False -> e
           };

sizeTree Empty = 0;
sizeTree (Node l i r) = (+) 1 ((+) (sizeTree l) (sizeTree r));

main = sizeTree (mkTree 30)

}
