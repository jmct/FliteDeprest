{
  map v10 v11 = case v11 of {
      Nil -> Nil;
      Cons v89 v90 ->
        Cons (v10 v89) (map v10 v90)
      };
  
  bestOf v14 v15 = case v14 of {
      Win -> Win;
      Loss -> v15;
      Draw -> case v15 of {
          Win -> Win;
          Draw -> Draw;
          Loss -> Draw
          }
      };
  
  inverse v16 = case v16 of {
      Loss -> Win;
      Draw -> Draw;
      Win -> Loss
      };
  
  fromTo v17 v18 = case
      ((<=) v17 v18) of {
      True ->
        Cons v17 (fromTo ((+) v17 1) v18);
      False -> Nil
      };
  
  cmp v19 v20 = case
      ((==) v19 v20) of {
      True -> EQ;
      False -> case ((<=) v19 v20) of
          {
          True -> LT;
          False -> GT
          }
      };
  
  insert v21 v22 = case v22 of {
      Nil -> Cons v21 Nil;
      Cons v91 v92 -> case
          ((<=) v21 v91) of {
          True -> Cons v21 (Cons v91 v92);
          False ->
            Cons v91 (insert v21 v92)
          }
      };
  
  foldr1 v25 v26 = case v26 of {
      Cons v93 v94 -> case v94 of {
          Nil -> v93;
          Cons v95 v96 ->
            v25 v93 (foldr1 v25 (Cons v95 v96))
          }
      };
  
  diff v31 v32 = case v31 of {
      Nil -> Nil;
      Cons v97 v98 -> case v32 of {
          Nil -> Cons v97 v98;
          Cons v99 v100 -> case
              (cmp v97 v99) of {
              LT ->
                Cons v97 (diff v98 (Cons v99 v100));
              EQ -> diff v98 v100;
              GT -> diff (Cons v97 v98) v100
              }
          }
      };
  
  null v37 = case v37 of {
      Nil -> True;
      Cons v101 v102 -> False
      };
  
  subset v40 v41
    = null (diff v40 v41);
  
  or v42 v43 = case v42 of {
      False -> v43;
      True -> True
      };
  
  hasLine v44
    = or (subset (Cons 1 (Cons 2 (Cons 3 Nil))) v44) (or (subset (Cons 4 (Cons 5 (Cons 6 Nil))) v44) (or (subset (Cons 7 (Cons 8 (Cons 9 Nil))) v44) (or (subset (Cons 1 (Cons 4 (Cons 7 Nil))) v44) (or (subset (Cons 2 (Cons 5 (Cons 8 Nil))) v44) (or (subset (Cons 3 (Cons 6 (Cons 9 Nil))) v44) (or (subset (Cons 1 (Cons 5 (Cons 9 Nil))) v44) (subset (Cons 3 (Cons 5 (Cons 7 Nil))) v44)))))));
  
  length v45 = lengthAcc 0 v45;
  
  lengthAcc v46 v47 = case v47 of
      {
      Nil -> v46;
      Cons v103 v104 ->
        lengthAcc ((+) 1 v46) v104
      };
  
  gridFull v50 v51
    = (==) (gridFull^^0 (length v50) (length v51)) 9;
  
  gridFull^^0 v1 v2 = (+) v1 v2;
  
  analysis v52 v53 = case
      (hasLine v53) of {
      True -> Loss;
      False -> case (gridFull v52 v53)
        of {
          True -> Draw;
          False ->
            foldr1 bestOf (map (moveval v52 v53) (diff (diff (fromTo 1 9) v52) v53))
          }
      };
  
  moveval v54 v55 v56
    = inverse (analysis v55 (insert v56 v54));
  
  adjudicate v57 v58 = case
      (cmp (length v57) (length v58))
    of {
      GT ->
        report (analysis v58 v57) X;
      EQ -> case (hasLine v58) of {
          True -> report Win X;
          False -> case (hasLine v57) of {
              True -> report Win O;
              False ->
                report (analysis v58 v57) X
              }
          };
      LT ->
        report (analysis v57 v58) O
      };
  
  report v59 v60 = case v59 of {
      Loss -> side (opp v60);
      Win -> side v60;
      Draw -> 68
      };
  
  opp v61 = case v61 of {
      O -> X;
      X -> O
      };
  
  side v62 = case v62 of {
      O -> 79;
      X -> 88
      };
  
  main  = adjudicate Nil Nil
}
