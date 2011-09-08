{
  sum v0 = case v0 of {
      Nil -> 0;
      Cons v6 v7 -> (+) v6 (sum v7)
      };
  
  double v3 = (+) v3 v3;
  
  main 
    = double (sum (Cons 1 (Cons 2 Nil)))
}
