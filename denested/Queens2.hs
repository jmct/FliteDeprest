{
  tail v3 = case v3 of {
      Cons v76 v77 -> v77
      };
  
  one v6 v7 = case v7 of {
      Nil -> Nil;
      Cons v78 v79 -> case (v6 v78) of
          {
          True -> Cons v78 Nil;
          False -> one v6 v79
          }
      };
  
  map v10 v11 = case v11 of {
      Nil -> Nil;
      Cons v80 v81 ->
        Cons (v10 v80) (map v10 v81)
      };
  
  append v14 v15 = case v14 of {
      Nil -> v15;
      Cons v82 v83 ->
        Cons v82 (append v83 v15)
      };
  
  concatMap v18 v19 = case v19 of
      {
      Nil -> Nil;
      Cons v84 v85 ->
        append (v18 v84) (concatMap v18 v85)
      };
  
  length v22 = lengthAcc 0 v22;
  
  lengthAcc v23 v24 = case v24 of
      {
      Nil -> v23;
      Cons v86 v87 ->
        lengthAcc ((+) 1 v23) v87
      };
  
  replicate v27 v28 = case
      ((==) v27 0) of {
      True -> Nil;
      False ->
        Cons v28 (replicate ((-) v27 1) v28)
      };
  
  l  = 0;
  
  r  = 1;
  
  d  = 2;
  
  eq v29 v30 = (==) v29 v30;
  
  left v31
    = map (one (eq l)) (tail v31);
  
  right v32
    = Cons Nil (map (one (eq r)) v32);
  
  down v33 = map (one (eq d)) v33;
  
  merge v34 v35 = case v34 of {
      Nil -> Nil;
      Cons v88 v89 -> case v35 of {
          Nil -> Cons v88 v89;
          Cons v90 v91 ->
            Cons (append v88 v90) (merge v89 v91)
          }
      };
  
  next v40
    = merge (merge (down v40) (left v40)) (right v40);
  
  fill v41 = case v41 of {
      Nil -> Nil;
      Cons v92 v93 ->
        append (lrd v92 v93) (map (Cons v92) (fill v93))
      };
  
  lrd v44 v45 = case v44 of {
      Nil ->
        Cons (Cons (Cons l (Cons r (Cons d Nil))) v45) Nil;
      Cons v94 v95 -> Nil
      };
  
  solve v48 v49 = case
      ((==) v48 0) of {
      True -> Cons Nil Nil;
      False ->
        concatMap (sol ((-) v48 1)) (fill v49)
      };
  
  sol v50 v51
    = map (Cons v51) (solve v50 (next v51));
  
  nqueens v52
    = length (solve v52 (replicate v52 Nil));
  
  main  = nqueens 11
}
