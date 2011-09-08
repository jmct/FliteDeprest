{
  main  = case
      (cichelli (Cons (Cons 97 (Cons 115 Nil)) (Cons (Cons 99 (Cons 97 (Cons 115 (Cons 101 Nil)))) (Cons (Cons 99 (Cons 108 (Cons 97 (Cons 115 (Cons 115 Nil))))) (Cons (Cons 100 (Cons 97 (Cons 116 (Cons 97 Nil)))) (Cons (Cons 100 (Cons 101 (Cons 102 (Cons 97 (Cons 117 (Cons 108 (Cons 116 Nil))))))) (Cons (Cons 100 (Cons 101 (Cons 114 (Cons 105 (Cons 118 (Cons 105 (Cons 110 (Cons 103 Nil)))))))) (Cons (Cons 100 (Cons 111 Nil)) (Cons (Cons 101 (Cons 108 (Cons 115 (Cons 101 Nil)))) (Cons (Cons 104 (Cons 105 (Cons 100 (Cons 105 (Cons 110 (Cons 103 Nil)))))) (Cons (Cons 105 (Cons 102 Nil)) (Cons (Cons 105 (Cons 109 (Cons 112 (Cons 111 (Cons 114 (Cons 116 Nil)))))) (Cons (Cons 105 (Cons 110 Nil)) (Cons (Cons 105 (Cons 110 (Cons 102 (Cons 105 (Cons 120 Nil))))) (Cons (Cons 105 (Cons 110 (Cons 102 (Cons 105 (Cons 120 (Cons 108 Nil)))))) (Cons (Cons 105 (Cons 110 (Cons 102 (Cons 105 (Cons 120 (Cons 114 Nil)))))) (Cons (Cons 105 (Cons 110 (Cons 115 (Cons 116 (Cons 97 (Cons 110 (Cons 99 (Cons 101 Nil)))))))) (Cons (Cons 108 (Cons 101 (Cons 116 Nil))) (Cons (Cons 109 (Cons 111 (Cons 100 (Cons 117 (Cons 108 (Cons 101 Nil)))))) (Cons (Cons 110 (Cons 101 (Cons 119 (Cons 116 (Cons 121 (Cons 112 (Cons 101 Nil))))))) (Cons (Cons 111 (Cons 102 Nil)) (Cons (Cons 113 (Cons 117 (Cons 97 (Cons 108 (Cons 105 (Cons 102 (Cons 105 (Cons 101 (Cons 100 Nil))))))))) (Cons (Cons 116 (Cons 104 (Cons 101 (Cons 110 Nil)))) (Cons (Cons 116 (Cons 121 (Cons 112 (Cons 101 Nil)))) (Cons (Cons 119 (Cons 104 (Cons 101 (Cons 114 (Cons 101 Nil))))) Nil)))))))))))))))))))))))))
    of {
      Just v401 -> emitHashFun v401;
      Nothing -> 0
      };
  
  cichelli v188 = case
      (freqSorted (map^enKey v188)) of
      {
      Pair v399 v400 ->
        findhash v400 (blockedWith Nil v399)
      };
  
  emitHashFun v189 = case v189 of
      {
      Nil -> 0;
      Cons v402 v403 -> case v402 of {
          Pair v404 v405 ->
            (+) ((+) v404 v405) (emitHashFun v403)
          }
      };
  
  freqSorted v127 = let {
      v360 = freqTabOf v127
      } in
      (Pair (sorted^decreasingFrequencyIn v360 v127) (length v360));
  
  map^enKey v59 = case v59 of {
      Nil -> Nil;
      Cons v322 v323 ->
        Cons (enKey v322) (map^enKey v323)
      };
  
  findhash v155 v156 = case
      (hashes v155 (length v156) v156 (Hash (H Nothing Nothing Nil) Nil))
    of {
      Cons v376 v377 -> case v376 of {
          Hash v378 v379 -> Just v379
          };
      Nil -> Nothing
      };
  
  blockedWith v143 v144 = case
      v144 of {
      Nil -> Nil;
      Cons v370 v371 -> let {
          v372 = union v143 (ends v370);
          v373 = endsSubset v372
          } in
          (Cons v370 (append (filter v373 v371) (blockedWith v372 (filter^non v373 v371))))
      };
  
  freqTabOf v142
    = histo (concatMap^ends v142);
  
  sorted^decreasingFrequencyIn v128
    = foldr^ordins^decreasingFrequencyIn v128 Nil;
  
  length v49 = lengthAcc 0 v49;
  
  enKey v151
    = K v151 (head v151) (last v151) (length v151);
  
  hashes v157 v158 v159 v160
    = case v159 of {
      Nil -> Cons v160 Nil;
      Cons v380 v381 ->
        concatMap^hashes v157 v158 v381 (concatMap^insertKey v158 v380 (concatMap^assignUpto v157 (lastLetter v380) (assignUpto v157 (firstLetter v380) v160)))
      };
  
  union v97 v98
    = foldr^ins v97 v98;
  
  ends v112 = case v112 of {
      K v348 v349 v350 v351 ->
        Cons v349 (Cons v350 Nil)
      };
  
  endsSubset v149 v150
    = subset (ends v150) v149;
  
  append v54 v55 = case v54 of {
      Nil -> v55;
      Cons v320 v321 ->
        Cons v320 (append v321 v55)
      };
  
  filter v75 v76 = case v76 of {
      Nil -> Nil;
      Cons v330 v331 -> case
          (v75 v330) of {
          True ->
            Cons v330 (filter v75 v331);
          False -> filter v75 v331
          }
      };
  
  filter^non v147 v76 = case v76
    of {
      Nil -> Nil;
      Cons v330 v331 -> case
          (non v147 v330) of {
          True ->
            Cons v330 (filter^non v147 v331);
          False -> filter^non v147 v331
          }
      };
  
  histo v101
    = foldr^histins Nil v101;
  
  concatMap^ends v63 = case v63 of
      {
      Nil -> Nil;
      Cons v324 v325 ->
        append (ends v324) (concatMap^ends v325)
      };
  
  foldr^ordins^decreasingFrequencyIn v128 v71 v72
    = case v72 of {
      Nil -> v71;
      Cons v328 v329 ->
        ordins^decreasingFrequencyIn v128 v328 (foldr^ordins^decreasingFrequencyIn v128 v71 v329)
      };
  
  lengthAcc v50 v51 = case v51 of
      {
      Nil -> v50;
      Cons v318 v319 ->
        lengthAcc ((+) v50 1) v319
      };
  
  head v40 = case v40 of {
      Cons v312 v313 -> v312
      };
  
  last v43 = case v43 of {
      Cons v314 v315 -> case
          (null v315) of {
          True -> v314;
          False -> last v315
          }
      };
  
  concatMap^hashes v157 v158 v159 v63
    = case v63 of {
      Nil -> Nil;
      Cons v324 v325 ->
        append (hashes v157 v158 v159 v324) (concatMap^hashes v157 v158 v159 v325)
      };
  
  concatMap^insertKey v166 v167 v63
    = case v63 of {
      Nil -> Nil;
      Cons v324 v325 ->
        append (insertKey v166 v167 v324) (concatMap^insertKey v166 v167 v325)
      };
  
  concatMap^assignUpto v163 v164 v63
    = case v63 of {
      Nil -> Nil;
      Cons v324 v325 ->
        append (assignUpto v163 v164 v324) (concatMap^assignUpto v163 v164 v325)
      };
  
  lastLetter v122 = case v122 of {
      K v356 v357 v358 v359 -> v358
      };
  
  assignUpto v163 v164 v165 = case
      (assocm v164 (hashAssoc v165))
    of {
      Nothing ->
        map^assign v164 v165 (enumFromTo 0 v163);
      Just v382 -> Cons v165 Nil
      };
  
  firstLetter v117 = case v117 of
      {
      K v352 v353 v354 v355 -> v353
      };
  
  foldr^ins v71 v72 = case v72 of
      {
      Nil -> v71;
      Cons v328 v329 ->
        ins v328 (foldr^ins v71 v329)
      };
  
  subset v93 v94 = case v93 of {
      Nil -> True;
      Cons v340 v341 -> case
          (elem v340 v94) of {
          True -> subset v341 v94;
          False -> False
          }
      };
  
  non v147 v148 = case (v147 v148)
    of {
      True -> False;
      False -> True
      };
  
  foldr^histins v71 v72 = case v72
    of {
      Nil -> v71;
      Cons v328 v329 ->
        histins v328 (foldr^histins v71 v329)
      };
  
  ordins^decreasingFrequencyIn v128 v108 v109
    = case v109 of {
      Nil -> Cons v108 Nil;
      Cons v346 v347 -> case
          (decreasingFrequencyIn v128 v108 v346)
        of {
          True ->
            Cons v108 (Cons v346 v347);
          False ->
            Cons v346 (ordins^decreasingFrequencyIn v128 v108 v347)
          }
      };
  
  null v46 = case v46 of {
      Nil -> True;
      Cons v316 v317 -> False
      };
  
  insertKey v166 v167 v168 = case
      v168 of {
      Hash v383 v384 -> case
          (hinsert v166 (hash v384 v167) v383)
        of {
          Nothing -> Nil;
          Just v385 ->
            Cons (Hash v385 v384) Nil
          }
      };
  
  assocm v87 v88 = case v88 of {
      Nil -> Nothing;
      Cons v336 v337 -> case v336 of {
          Pair v338 v339 -> case
              ((==) v87 v338) of {
              True -> Just v339;
              False -> assocm v87 v337
              }
          }
      };
  
  hashAssoc v152 = case v152 of {
      Hash v374 v375 -> v375
      };
  
  map^assign v171 v172 v59 = case
      v59 of {
      Nil -> Nil;
      Cons v322 v323 ->
        Cons (assign v171 v172 v322) (map^assign v171 v172 v323)
      };
  
  enumFromTo v79 v80 = case
      ((<=) v79 v80) of {
      True ->
        Cons v79 (enumFromTo ((+) v79 1) v80);
      False -> Nil
      };
  
  ins v99 v100 = case
      (elem v99 v100) of {
      True -> v100;
      False -> Cons v99 v100
      };
  
  elem v66 v67 = case v67 of {
      Nil -> False;
      Cons v326 v327 -> case
          ((==) v66 v326) of {
          True -> True;
          False -> elem v66 v327
          }
      };
  
  histins v102 v103 = case v103 of
      {
      Nil -> Cons (Pair v102 1) Nil;
      Cons v342 v343 -> case v342 of {
          Pair v344 v345 -> case
              ((==) v102 v344) of {
              True ->
                Cons (Pair v344 ((+) v345 1)) v343;
              False ->
                Cons v342 (histins v102 v343)
              }
          }
      };
  
  decreasingFrequencyIn v128 v129 v130
    = case v129 of {
      K v361 v362 v363 v364 -> case
          v130 of {
          K v365 v366 v367 v368 -> let {
              v369 = flip^assoc v128
              } in
              (gt ((+) (v369 v362) (v369 v363)) ((+) (v369 v366) (v369 v367)))
          }
      };
  
  hinsert v176 v177 v178 = case
      v178 of {
      H v388 v389 v390 -> let {
          v393 = case v388 of {
            Nothing -> v177;
            Just v391 -> min v391 v177
            };
          v394 = case v389 of {
            Nothing -> v177;
            Just v392 -> max v392 v177
            }
          } in (case (elem v177 v390) of {
            True -> Nothing;
            False -> case
                ((<=) ((-) ((+) v394 1) v393) v176)
              of {
                False -> Nothing;
                True ->
                  Just (H (Just v393) (Just v394) (Cons v177 v390))
                }
            })
      };
  
  hash v182 v183 = case v183 of {
      K v395 v396 v397 v398 ->
        (+) v398 ((+) (assoc v396 v182) (assoc v397 v182))
      };
  
  assign v171 v172 v173 = case
      v172 of {
      Hash v386 v387 ->
        Hash v386 (Cons (Pair v171 v173) v387)
      };
  
  flip^assoc v140 v141
    = assoc v141 v140;
  
  gt v38 v39 = case ((<=) v38 v39)
    of {
      True -> False;
      False -> True
      };
  
  min v34 v35 = case
      ((<=) v34 v35) of {
      True -> v34;
      False -> v35
      };
  
  max v36 v37 = case
      ((<=) v36 v37) of {
      True -> v37;
      False -> v36
      };
  
  assoc v81 v82 = case v82 of {
      Cons v332 v333 -> case v332 of {
          Pair v334 v335 -> case
              ((==) v81 v334) of {
              True -> v335;
              False -> assoc v81 v333
              }
          }
      }
}
