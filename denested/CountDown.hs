{
  valid v21 v22 v23 = case v21
    of {
      Add -> True;
      Sub -> not ((<=) v22 v23);
      Mul -> True;
      Div -> (==) (mod v22 v23) 0
      };
  
  apply v24 v25 v26 = case v24 of
      {
      Add -> (+) v25 v26;
      Sub -> (-) v25 v26;
      Mul -> mul v25 v26;
      Div -> div v25 v26
      };
  
  subs v27 = case v27 of {
      Nil -> Cons Nil Nil;
      Cons v173 v174 -> let {
          v175 = subs v174
          } in
          (append v175 (map (Cons v173) v175))
      };
  
  interleave v30 v31 = case v31 of
      {
      Nil -> Cons (Cons v30 Nil) Nil;
      Cons v176 v177 ->
        Cons (Cons v30 (Cons v176 v177)) (map (Cons v176) (interleave v30 v177))
      };
  
  perms v34 = case v34 of {
      Nil -> Cons Nil Nil;
      Cons v178 v179 ->
        concatMap (interleave v178) (perms v179)
      };
  
  choices v37
    = concatMap perms (subs v37);
  
  split v38 = case v38 of {
      Cons v180 v181 -> case
          (null v181) of {
          True -> Nil;
          False ->
            Cons (Pair (Cons v180 Nil) v181) (map (cross (Pair (Cons v180) id)) (split v181))
          }
      };
  
  results v41 = case v41 of {
      Nil -> Nil;
      Cons v182 v183 -> case
          (null v183) of {
          True ->
            Cons (Pair (Val v182) v182) Nil;
          False ->
            concatMap combinedResults (split (Cons v182 v183))
          }
      };
  
  combinedResults v44 = case v44
    of {
      Pair v184 v185 ->
        concatProdWith combine (results v184) (results v185)
      };
  
  concatProdWith v47 v48 v49
    = case v48 of {
      Nil -> Nil;
      Cons v186 v187 ->
        append (concatMap (v47 v186) v49) (concatProdWith v47 v187 v49)
      };
  
  combine v52 v53 = case v52 of {
      Pair v188 v189 -> case v53 of {
          Pair v190 v191 ->
            concatMap (combi v188 v189 v190 v191) (Cons Add (Cons Sub (Cons Mul (Cons Div Nil))))
          }
      };
  
  combi v58 v59 v60 v61 v62 = case
      (valid v62 v59 v61) of {
      True ->
        Cons (Pair (App v62 v58 v60) (apply v62 v59 v61)) Nil;
      False -> Nil
      };
  
  solutions v63 v64
    = concatMap (solns v64) (choices v63);
  
  solns v65 v66
    = preImage v65 (results v66);
  
  preImage v67 v68 = case v68 of {
      Nil -> Nil;
      Cons v192 v193 -> case v192 of {
          Pair v194 v195 -> case
              ((==) v195 v67) of {
              True ->
                Cons v194 (preImage v67 v193);
              False -> preImage v67 v193
              }
          }
      };
  
  not v73 = case v73 of {
      True -> False;
      False -> True
      };
  
  div v74 v75 = case
      (divMod v74 v75) of {
      Pair v196 v197 -> v196
      };
  
  mod v76 v77 = case
      (divMod v76 v77) of {
      Pair v198 v199 -> v199
      };
  
  divMod v78 v79 = let {
      v203 = (+) v79 v79
      } in (case ((<=) v203 v78) of {
        True -> case (divMod v78 v203)
          of {
            Pair v200 v201 -> let {
                v202 = (+) v200 v200
                } in (case ((<=) v79 v201) of {
                  True ->
                    Pair ((+) 1 v202) ((-) v201 v79);
                  False -> Pair v202 v201
                  })
            };
        False -> case ((<=) v79 v78) of
            {
            True -> Pair 1 ((-) v78 v79);
            False -> Pair 0 v78
            }
        });
  
  mul v80 v81 = case ((==) v81 1)
    of {
      True -> v80;
      False -> case (divMod v81 2) of
          {
          Pair v204 v205 ->
            (+) (mul ((+) v80 v80) v204) (case
              ((==) v205 0) of {
              True -> 0;
              False -> v80
              })
          }
      };
  
  cross v82 v83 = case v82 of {
      Pair v206 v207 -> case v83 of {
          Pair v208 v209 ->
            Pair (v206 v208) (v207 v209)
          }
      };
  
  id v88 = v88;
  
  null v89 = case v89 of {
      Nil -> True;
      Cons v210 v211 -> False
      };
  
  length v92 = lengthAcc 0 v92;
  
  lengthAcc v93 v94 = case v94 of
      {
      Nil -> v93;
      Cons v212 v213 ->
        lengthAcc ((+) v93 1) v213
      };
  
  append v97 v98 = case v97 of {
      Nil -> v98;
      Cons v214 v215 ->
        Cons v214 (append v215 v98)
      };
  
  map v101 v102 = case v102 of {
      Nil -> Nil;
      Cons v216 v217 ->
        Cons (v101 v216) (map v101 v217)
      };
  
  concatMap v105 v106 = case v106
    of {
      Nil -> Nil;
      Cons v218 v219 ->
        append (v105 v218) (concatMap v105 v219)
      };
  
  main 
    = length (solutions (Cons 1 (Cons 3 (Cons 7 (Cons 10 (Cons 25 Nil))))) 765)
}
