{
  p v6 = length (partitions v6);
  
  partitions v7
    = partitionsWith v7 (countDown v7);
  
  partitionsWith v8 v9 = case
      ((==) v8 0) of {
      True -> Cons Nil Nil;
      False ->
        concatMap (partitionsWith0 v8 v9) v9
      };
  
  and v10 v11 = case v10 of {
      False -> False;
      True -> v11
      };
  
  lt v12 v13
    = and ((/=) v12 v13) ((<=) v12 v13);
  
  partitionsWith0 v14 v15 v16
    = let {
      v55 = (-) v14 v16
      } in
      (map (Cons v16) (partitionsWith v55 (dropWhile (lt (min v16 v55)) v15)));
  
  length v17 = lengthAcc 0 v17;
  
  lengthAcc v18 v19 = case v19 of
      {
      Nil -> v18;
      Cons v56 v57 ->
        lengthAcc ((+) v18 1) v57
      };
  
  countDown v22 = case
      ((<=) 1 v22) of {
      True ->
        Cons v22 (countDown ((-) v22 1));
      False -> Nil
      };
  
  concatMap v23 v24 = case v24 of
      {
      Nil -> Nil;
      Cons v58 v59 ->
        append (v23 v58) (concatMap v23 v59)
      };
  
  append v27 v28 = case v27 of {
      Nil -> v28;
      Cons v60 v61 ->
        Cons v60 (append v61 v28)
      };
  
  min v31 v32 = case
      ((<=) v31 v32) of {
      True -> v31;
      False -> v32
      };
  
  map v33 v34 = case v34 of {
      Nil -> Nil;
      Cons v62 v63 ->
        Cons (v33 v62) (map v33 v63)
      };
  
  dropWhile v37 v38 = case v38 of
      {
      Nil -> Nil;
      Cons v64 v65 -> case (v37 v64)
        of {
          True -> dropWhile v37 v65;
          False -> v38
          }
      };
  
  main  = p 30
}
