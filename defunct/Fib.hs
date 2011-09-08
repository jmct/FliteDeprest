{
  fib v1 = case ((<=) v1 1) of {
      True -> 1;
      False ->
        (+) (fib ((-) v1 2)) (fib ((-) v1 1))
      };
  
  main  = fib 36
}
