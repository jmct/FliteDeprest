{
  insert v3 v4 = case v4 of {
      Empty -> Branch v3 Empty Empty;
      Branch v72 v73 v74 ->
        Branch v3 (insert v72 v74) v73
      };
  
  and v8 v9 = case v8 of {
      False -> False;
      True -> v9
      };
  
  size v10 = case v10 of {
      Empty -> 0;
      Branch v75 v76 v77 ->
        (+) 1 (size^^0 (size v76) (size v77))
      };
  
  size^^0 v1 v2 = (+) v1 v2;
  
  fromList v14 = case v14 of {
      Nil -> Empty;
      Cons v78 v79 ->
        insert v78 (fromList v79)
      };
  
  toList v17 = case v17 of {
      Empty -> Nil;
      Branch v80 v81 v82 ->
        Cons v80 (ilv (toList v81) (toList v82))
      };
  
  ilv v21 v22 = case v21 of {
      Nil -> v22;
      Cons v83 v84 -> case v22 of {
          Nil -> Cons v83 v84;
          Cons v85 v86 ->
            Cons v83 (Cons v85 (ilv v84 v86))
          }
      };
  
  replicate v27 v28 = case
      ((==) v27 0) of {
      True -> Nil;
      False ->
        Cons v28 (replicate ((-) v27 1) v28)
      };
  
  fromTo v29 v30 = case
      ((<=) v29 v30) of {
      True ->
        Cons v29 (fromTo ((+) v29 1) v30);
      False -> Nil
      };
  
  repeat v31
    = Cons v31 (repeat v31);
  
  equal v32 v33 = case v32 of {
      Nil -> case v33 of {
          Nil -> True;
          Cons v87 v88 -> False
          };
      Cons v89 v90 -> case v33 of {
          Nil -> False;
          Cons v91 v92 -> case
              ((==) v89 v91) of {
              False -> False;
              True -> equal v90 v92
              }
          }
      };
  
  prop v40
    = equal v40 (toList (fromList v40));
  
  all v41 v42 = case v42 of {
      Nil -> True;
      Cons v93 v94 ->
        and (v41 v93) (all v41 v94)
      };
  
  int v45 = case v45 of {
      True -> 1;
      False -> 0
      };
  
  main 
    = int (all prop (replicate 6000 (fromTo 0 255)))
}
