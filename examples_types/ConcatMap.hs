{

--data Bool    = True | False ;
--data List a  = Cons a (List a)|Nil; 


--map f Nil = Nil;
--map f (Cons x xs) = Cons (f x) (map f xs);

--append Nil ys = ys;
--append (Cons x xs) ys = Cons x (append xs ys);

--concatMap f Nil = Nil;
--concatMap f (Cons x xs) = append (f x) (concatMap f xs);

--main = 1;
id x = x;
main = id 5;
}


*** Exception: 

FUNCTION::TYPE 

main :: Int
nsoln :: Int -> Int
gen :: Int -> Int -> [[Int]]
gen1 :: Int -> [Int] -> [[Int]]
gen2 :: [Int] -> Int -> [[Int]]
safe :: Int -> Int -> [Int] -> Bool
concatMap :: (a -> [b]) -> [a] -> [b]
toOne :: Int -> [Int]
length :: [a] -> Int
append :: [a] -> [a] -> [a]
map :: (a -> b) -> [a] -> [b]
and :: Bool -> Bool -> Bool
True :: Bool
False :: Bool
Cons :: a -> [a] -> [a]
Nil :: [a]
Pair :: (a,b)
(+) :: Int -> Int -> Int
(-) :: Int -> Int -> Int
(==) :: Int -> Int -> Bool
(/=) :: Int -> Int -> Bool
(<=) :: Int -> Int -> Bool
emitInt :: Int -> a -> a
emit :: Int -> a -> a
[]

