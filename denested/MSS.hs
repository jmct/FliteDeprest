{
  init v4 = case v4 of {
      Cons v63 v64 -> case v64 of {
          Nil -> Nil;
          Cons v65 v66 ->
            Cons v63 (init (Cons v65 v66))
          }
      };
  
  inits v9 = case v9 of {
      Nil -> Cons Nil Nil;
      Cons v67 v68 ->
        Cons v9 (inits (init v9))
      };
  
  tails v10 = case v10 of {
      Nil -> Nil;
      Cons v69 v70 ->
        Cons (Cons v69 v70) (tails v70)
      };
  
  map v13 v14 = case v14 of {
      Nil -> Nil;
      Cons v71 v72 ->
        Cons (v13 v71) (map v13 v72)
      };
  
  append v17 v18 = case v17 of {
      Nil -> v18;
      Cons v73 v74 ->
        Cons v73 (append v74 v18)
      };
  
  concatMap v21 v22 = case v22 of
      {
      Nil -> Nil;
      Cons v75 v76 ->
        append (v21 v75) (concatMap v21 v76)
      };
  
  segments v25
    = concatMap tails (inits v25);
  
  maximum v26 = case v26 of {
      Cons v77 v78 -> max v77 v78
      };
  
  max v29 v30 = case v30 of {
      Nil -> v29;
      Cons v79 v80 -> case
          ((<=) v29 v79) of {
          True -> max v79 v80;
          False -> max v29 v80
          }
      };
  
  sum v33 = sumAcc 0 v33;
  
  sumAcc v34 v35 = case v35 of {
      Nil -> v34;
      Cons v81 v82 ->
        sumAcc ((+) v81 v34) v82
      };
  
  mss v38
    = maximum (map sum (segments v38));
  
  fromTo v39 v40 = case
      ((<=) v39 v40) of {
      True ->
        Cons v39 (fromTo ((+) v39 1) v40);
      False -> Nil
      };
  
  main 
    = (+) (main^^0 (mss (fromTo ((-) 0 160) 160)) (mss (fromTo ((-) 0 150) 150))) (main^^1 (mss (fromTo ((-) 0 161) 161)) (mss (fromTo ((-) 0 151) 151)));
  
  main^^0 v1 v2 = (-) v1 v2;
  
  main^^1 v1 v2 = (-) v1 v2
}
