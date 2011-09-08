{
  implies v2 v3 = case v2 of {
      False -> True;
      True -> v3
      };
  
  and v4 v5 = case v4 of {
      False -> False;
      True -> v5
      };
  
  andList v6 = case v6 of {
      Nil -> True;
      Cons v47 v48 ->
        and v47 (andList v48)
      };
  
  append v9 v10 = case v9 of {
      Nil -> v10;
      Cons v49 v50 ->
        Cons v49 (append v50 v10)
      };
  
  map v13 v14 = case v14 of {
      Nil -> Nil;
      Cons v51 v52 ->
        Cons (v13 v51) (map v13 v52)
      };
  
  ord v17 = case v17 of {
      Nil -> True;
      Cons v53 v54 -> case v54 of {
          Nil -> True;
          Cons v55 v56 ->
            and (implies v53 v55) (ord (Cons v55 v56))
          }
      };
  
  insert v22 v23 = case v23 of {
      Nil -> Cons v22 Nil;
      Cons v57 v58 -> case
          (implies v22 v57) of {
          True -> Cons v22 (Cons v57 v58);
          False ->
            Cons v57 (insert v22 v58)
          }
      };
  
  prop v26 v27
    = implies (ord v27) (ord (insert v26 v27));
  
  boolList v28 = case v28 of {
      Z -> Cons Nil Nil;
      S v59 ->
        append (boolList v59) (append (map (Cons False) (boolList v59)) (map (Cons True) (boolList v59)))
      };
  
  top v30
    = andList (append (map (prop True) (boolList v30)) (map (prop False) (boolList v30)));
  
  main  = case
      (top (S (S (S (S (S (S (S (S (S (S (S (S Z)))))))))))))
    of {
      False -> 0;
      True -> 1
      }
}
