{
  main  = let {
      v252 = Cons (Cons 65 (Cons 78 (Cons 65 (Cons 78 (Cons 65 (Cons 66 Nil)))))) (Cons (Cons 69 (Cons 76 (Cons 80 (Cons 80 (Cons 65 Nil))))) (Cons (Cons 89 (Cons 82 (Cons 82 (Cons 69 (Cons 72 (Cons 67 Nil)))))) (Cons (Cons 72 (Cons 67 (Cons 65 (Cons 69 (Cons 80 Nil))))) (Cons (Cons 84 (Cons 79 (Cons 67 (Cons 73 (Cons 82 (Cons 80 (Cons 65 Nil))))))) (Cons (Cons 69 (Cons 86 (Cons 73 (Cons 76 (Cons 79 Nil))))) (Cons (Cons 78 (Cons 79 (Cons 77 (Cons 69 (Cons 76 Nil))))) (Cons (Cons 65 (Cons 86 (Cons 65 (Cons 85 (Cons 71 Nil))))) (Cons (Cons 79 (Cons 68 (Cons 65 (Cons 67 (Cons 65 (Cons 86 (Cons 65 Nil))))))) (Cons (Cons 65 (Cons 89 (Cons 65 (Cons 80 (Cons 65 (Cons 80 Nil)))))) (Cons (Cons 73 (Cons 72 (Cons 67 (Cons 84 (Cons 73 (Cons 76 Nil)))))) (Cons (Cons 78 (Cons 79 (Cons 76 (Cons 69 (Cons 77 Nil))))) Nil)))))))))))
      } in (count v252 v252 v252);
  
  count v121 v122 v123
    = sumMap^fx v122 v123 v121;
  
  sumMap^fx v124 v125 v115
    = sumMapAcc^fx v124 v125 v115 0;
  
  sumMapAcc^fx v124 v125 v117 v118
    = case v117 of {
      Nil -> v118;
      Cons v248 v249 ->
        sumMapAcc^fx v124 v125 v249 ((+) (fx v124 v125 v248) v118)
      };
  
  fx v124 v125 v126
    = sumMap^fy v126 v125 v124;
  
  sumMap^fy v127 v128 v115
    = sumMapAcc^fy v127 v128 v115 0;
  
  sumMapAcc^fy v127 v128 v117 v118
    = case v117 of {
      Nil -> v118;
      Cons v248 v249 ->
        sumMapAcc^fy v127 v128 v249 ((+) (fy v127 v128 v248) v118)
      };
  
  fy v127 v128 v129
    = sumMap^fz v127 v129 v128;
  
  sumMap^fz v130 v131 v115
    = sumMapAcc^fz v130 v131 v115 0;
  
  sumMapAcc^fz v130 v131 v117 v118
    = case v117 of {
      Nil -> v118;
      Cons v248 v249 ->
        sumMapAcc^fz v130 v131 v249 ((+) (fz v130 v131 v248) v118)
      };
  
  fz v130 v131 v132 = case
      (valid v130 v131 v132) of {
      True -> 1;
      False -> 0
      };
  
  valid v111 v112 v113
    = and ((==) (length v111) (length v112)) (and ((==) (length v111) (length v113)) (isSingleton (solutions v111 v112 v113 (Pair 0 Nil))));
  
  and v109 v110 = case v109 of {
      False -> False;
      True -> v110
      };
  
  length v133 = lengthAcc 0 v133;
  
  isSingleton v104 = case v104 of
      {
      Nil -> False;
      Cons v244 v245 -> case v245 of {
          Nil -> True;
          Cons v246 v247 -> False
          }
      };
  
  solutions v82 v83 v84 v85 = case
      v82 of {
      Nil -> case v84 of {
          Nil -> case ((==) (fst v85) 0)
            of {
              False -> Nil;
              True -> Cons (snd v85) Nil
              };
          Cons v235 v236 -> case v236 of {
              Nil -> case ((==) (fst v85) 1)
                of {
                  False -> Nil;
                  True ->
                    bindings v235 (Cons 1 Nil) (snd v85)
                  }
              }
          };
      Cons v237 v238 -> case v84 of {
          Cons v239 v240 ->
            ofAll^solns (fst v85) v237 (head v83) v239 (solutions v238 (tail v83) v240) (ofAll^bindings (head v83) (fromTo (ifNull (tail v83) 1 0) 9) (bindings v237 (fromTo (ifNull v238 1 0) 9) (snd v85)))
          }
      };
  
  lengthAcc v134 v135 = case v135
    of {
      Nil -> v134;
      Cons v250 v251 ->
        lengthAcc ((+) v134 1) v251
      };
  
  fst v14 = case v14 of {
      Pair v201 v202 -> v201
      };
  
  snd v17 = case v17 of {
      Pair v203 v204 -> v204
      };
  
  bindings v60 v61 v62 = case
      (lookup v60 v62) of {
      Nothing ->
        map^flip^Cons v62 (zip (repeat v60) (foldl^flip^del v61 (map^snd v62)));
      Just v224 -> case
          (member v224 v61) of {
          True -> Cons v62 Nil;
          False -> Nil
          }
      };
  
  ofAll^solns v95 v96 v97 v98 v99 v68
    = case v68 of {
      Nil -> Nil;
      Cons v227 v228 ->
        append (solns v95 v96 v97 v98 v99 v227) (ofAll^solns v95 v96 v97 v98 v99 v228)
      };
  
  head v76 = case v76 of {
      Cons v231 v232 -> v231
      };
  
  tail v79 = case v79 of {
      Cons v233 v234 -> v234
      };
  
  ofAll^bindings v60 v61 v68
    = case v68 of {
      Nil -> Nil;
      Cons v227 v228 ->
        append (bindings v60 v61 v227) (ofAll^bindings v60 v61 v228)
      };
  
  fromTo v102 v103
    = Cons v102 (case
      ((==) v102 v103) of {
      True -> Nil;
      False ->
        fromTo ((+) v102 1) v103
      });
  
  ifNull v71 v72 v73 = case v71 of
      {
      Nil -> v72;
      Cons v229 v230 -> v73
      };
  
  lookup v24 v25 = case v25 of {
      Nil -> Nothing;
      Cons v206 v207 -> case v206 of {
          Pair v208 v209 -> case
              ((==) v24 v208) of {
              False -> lookup v24 v207;
              True -> Just v209
              }
          }
      };
  
  map^flip^Cons v42 v57 = case v57
    of {
      Nil -> Nil;
      Cons v222 v223 ->
        Cons (flip^Cons v42 v222) (map^flip^Cons v42 v223)
      };
  
  zip v50 v51 = case v50 of {
      Nil -> Nil;
      Cons v218 v219 -> case v51 of {
          Nil -> Nil;
          Cons v220 v221 ->
            Cons (Pair v218 v220) (zip v219 v221)
          }
      };
  
  repeat v49
    = Cons v49 (repeat v49);
  
  foldl^flip^del v45 v46 = case
      v46 of {
      Nil -> v45;
      Cons v216 v217 ->
        foldl^flip^del (flip^del v45 v216) v217
      };
  
  map^snd v57 = case v57 of {
      Nil -> Nil;
      Cons v222 v223 ->
        Cons (snd v222) (map^snd v223)
      };
  
  member v37 v38 = case v38 of {
      Nil -> False;
      Cons v214 v215 -> case
          ((==) v37 v214) of {
          False -> member v37 v215;
          True -> True
          }
      };
  
  append v63 v64 = case v63 of {
      Nil -> v64;
      Cons v225 v226 ->
        Cons v225 (append v226 v64)
      };
  
  solns v95 v96 v97 v98 v99 v100
    = let {
      v241 = divMod10 ((+) ((+) (img v100 v96) (img v100 v97)) v95)
      } in
      (ofAll^curry v99 (fst v241) (bindings v98 (Cons (snd v241) Nil) v100));
  
  flip^Cons v42 v43
    = Cons v43 v42;
  
  flip^del v42 v43 = del v43 v42;
  
  divMod10 v101 = case
      ((<=) v101 9) of {
      True -> Pair 0 v101;
      False -> case
          (divMod10 ((-) v101 10)) of {
          Pair v242 v243 ->
            Pair ((+) v242 1) v243
          }
      };
  
  img v20 v21
    = fromJust (lookup v21 v20);
  
  ofAll^curry v92 v93 v68 = case
      v68 of {
      Nil -> Nil;
      Cons v227 v228 ->
        append (curry v92 v93 v227) (ofAll^curry v92 v93 v228)
      };
  
  del v33 v34 = case v34 of {
      Nil -> Nil;
      Cons v212 v213 -> case
          ((==) v33 v212) of {
          False ->
            Cons v212 (del v33 v213);
          True -> v213
          }
      };
  
  fromJust v22 = case v22 of {
      Just v205 -> v205
      };
  
  curry v92 v93 v94
    = v92 (Pair v93 v94)
}
