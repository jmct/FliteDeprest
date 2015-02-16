{
data Bool    = True | False ;
data List a  = Cons a (List a)|Nil; 
data U       = Unit;

fib n = if (<=) n 1 then 1 else (+) (fib ((-) n 2)) (fib ((-) n 1));

main = fib 10;

}
