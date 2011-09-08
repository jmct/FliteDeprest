{
  main 
    = value (ssos (Comp (Ass 4 (V 3)) (While (Neg (Eq (V 4) (N 0))) (Comp (Comp (Comp (Comp (Ass 0 (V 3)) (Ass 1 (V 4))) (While (Le (V 1) (V 0)) (Comp (Ass 0 (Sub (V 0) (V 1))) (Ass 2 (Add (V 2) (N 1)))))) (If (Eq (V 0) (N 0)) (Ass 5 (Add (V 5) (N 1))) Skip)) (Ass 4 (Sub (V 4) (N 1)))))) (Cons (Pair 0 0) (Cons (Pair 1 0) (Cons (Pair 2 0) (Cons (Pair 3 14000) (Cons (Pair 4 0) (Cons (Pair 5 0) Nil))))))) 5 id;
  
  value v7 v8 v9 = case v7 of {
      Cons v141 v142 -> case v141 of {
          Pair v143 v144 -> case
              ((==) v143 v8) of {
              True -> v9 v144;
              False -> value v142 v8 v9
              }
          }
      };
  
  ssos v94 v95
    = run (Inter v94 v95);
  
  id v96 = v96;
  
  run v90 = case v90 of {
      Inter v174 v175 ->
        run (sosstm v174 v175);
      Final v176 -> v176
      };
  
  sosstm v75 v76 = case v75 of {
      Ass v162 v163 ->
        aval v163 v76 (update v76 v162 Final);
      Skip -> Final v76;
      Comp v164 v165 -> case
          (sosstm v164 v76) of {
          Inter v166 v167 ->
            Inter (Comp v166 v165) v167;
          Final v168 -> Inter v165 v168
          };
      If v169 v170 v171 ->
        bval v169 v76 (cond v76 v170 v171);
      While v172 v173 ->
        Inter (If v172 (Comp v173 (While v172 v173)) Skip) v76
      };
  
  aval v56 v57 v58 = case v56 of {
      N v149 -> v58 v149;
      V v150 -> value v57 v150 v58;
      Add v151 v152 ->
        seq^aval v151 v57 (aval v152 v57) (add v58);
      Sub v153 v154 ->
        seq^aval v153 v57 (aval v154 v57) (sub v58)
      };
  
  update v14 v15 v16 v17 = case
      v14 of {
      Nil -> v16 Nil;
      Cons v145 v146 -> case v145 of {
          Pair v147 v148 -> case
              ((==) v147 v15) of {
              True ->
                update v146 v15 (upd v16 v15 v17) v17;
              False ->
                update v146 v15 (upd v16 v147 v148) v17
              }
          }
      };
  
  bval v65 v66 v67 = case v65 of {
      TRUE -> v67 True;
      FALSE -> v67 False;
      Eq v155 v156 ->
        seq^aval v155 v66 (aval v156 v66) (eq v67);
      Le v157 v158 ->
        seq^aval v157 v66 (aval v158 v66) (leq v67);
      Neg v159 ->
        bval v159 v66 (notk v67);
      And v160 v161 ->
        seq^bval v160 v66 (bval v161 v66) (andk v67)
      };
  
  cond v86 v87 v88 v89 = case v89
    of {
      True -> Inter v87 v86;
      False -> Inter v88 v86
      };
  
  seq^aval v56 v57 v51 v52
    = aval v56 v57 (comp v51 v52);
  
  add v33 v34 v35
    = int ((+) v34 v35) v33;
  
  sub v36 v37 v38
    = int ((-) v37 v38) v36;
  
  upd v22 v23 v24 v25
    = v22 (Cons (Pair v23 v24) v25);
  
  eq v39 v40 v41
    = bool ((==) v40 v41) v39;
  
  leq v42 v43 v44
    = bool ((<=) v43 v44) v42;
  
  notk v45 v46
    = bool (not v46) v45;
  
  seq^bval v65 v66 v51 v52
    = bval v65 v66 (comp v51 v52);
  
  andk v47 v48 v49
    = bool (and v48 v49) v47;
  
  comp v53 v54 v55
    = v53 (v54 v55);
  
  int v26 v27 = case ((==) v26 0)
    of {
      True -> v27 0;
      False -> v27 v26
      };
  
  bool v28 v29 = case v28 of {
      False -> v29 False;
      True -> v29 True
      };
  
  not v30 = case v30 of {
      False -> True;
      True -> False
      };
  
  and v31 v32 = case v31 of {
      False -> False;
      True -> v32
      }
}
