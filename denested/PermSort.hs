{
  and v1 v2 = case v1 of {
      False -> False;
      True -> v2
      };
  
  head v3 = case v3 of {
      Cons v54 v55 -> v54
      };
  
  map v6 v7 = case v7 of {
      Nil -> Nil;
      Cons v56 v57 ->
        Cons (v6 v56) (map v6 v57)
      };
  
  append v10 v11 = case v10 of {
      Nil -> v11;
      Cons v58 v59 ->
        Cons v58 (append v59 v11)
      };
  
  concatMap v14 v15 = case v15 of
      {
      Nil -> Nil;
      Cons v60 v61 ->
        append (v14 v60) (concatMap v14 v61)
      };
  
  filter v18 v19 = case v19 of {
      Nil -> Nil;
      Cons v62 v63 -> case (v18 v62)
        of {
          True ->
            Cons v62 (filter v18 v63);
          False -> filter v18 v63
          }
      };
  
  place v22 v23 = case v23 of {
      Nil -> Cons (Cons v22 Nil) Nil;
      Cons v64 v65 ->
        Cons (Cons v22 (Cons v64 v65)) (map (Cons v64) (place v22 v65))
      };
  
  perm v26 = case v26 of {
      Nil -> Cons Nil Nil;
      Cons v66 v67 ->
        concatMap (place v66) (perm v67)
      };
  
  ord v29 = case v29 of {
      Nil -> True;
      Cons v68 v69 -> case v69 of {
          Nil -> True;
          Cons v70 v71 ->
            and ((<=) v68 v70) (ord (Cons v70 v71))
          }
      };
  
  permSort v34
    = head (filter ord (perm v34));
  
  main 
    = head (permSort (Cons 10 (Cons 9 (Cons 8 (Cons 7 (Cons 6 (Cons 5 (Cons 4 (Cons 3 (Cons 2 (Cons 1 Nil)))))))))))
}
