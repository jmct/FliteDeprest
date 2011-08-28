{

sumAcc acc Nil = acc;
sumAcc acc (Cons x xs) = sumAcc (acc+x) xs;

main = sumAcc 0 (Cons 1 (Cons 2 (Cons 3 Nil)))

}
