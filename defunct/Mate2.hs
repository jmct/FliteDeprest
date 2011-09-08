{
  main 
    = solveProblem (Pair (Board (Map Knight (Pair 7 8) (Map Rook (Pair 5 7) (Map King (Pair 8 7) (Map Bishop (Pair 4 5) (Map Pawn (Pair 8 4) (Map Pawn (Pair 7 3) (Map Pawn (Pair 5 2) (Map Pawn (Pair 6 2) (Map Queen (Pair 5 1) Empty))))))))) (Map Knight (Pair 2 8) (Map Pawn (Pair 7 7) (Map Pawn (Pair 4 6) (Map Pawn (Pair 3 5) (Map King (Pair 6 5) (Map Pawn (Pair 8 5) (Map Pawn (Pair 4 4) (Map Pawn (Pair 2 3) (Map Pawn (Pair 5 3) (Map Pawn (Pair 7 2) (Map Queen (Pair 1 1) (Map Knight (Pair 2 1) (Map Bishop (Pair 8 1) Empty)))))))))))))) (Pair White 3));
  
  solveProblem v308 = case v308 of
      {
      Pair v623 v624 -> case v624 of {
          Pair v625 v626 ->
            solve v623 v625 v626
          }
      };
  
  solve v282 v283 v284
    = showResult (solution v282 v283 ((-) ((+) v284 v284) 1));
  
  showResult v303 = case v303 of {
      Nothing -> 0;
      Just v620 -> size v620
      };
  
  solution v285 v286 v287
    = foldr^solnOr v286 v287 Nothing (moveDetailsFor v286 v285);
  
  size v305 = case v305 of {
      Solution v621 v622 ->
        (+) 1 (sum (map^size (snd (unzip v622))))
      };
  
  foldr^solnOr v288 v289 v94 v95
    = case v95 of {
      Nil -> v94;
      Cons v512 v513 ->
        solnOr v288 v289 v512 (foldr^solnOr v288 v289 v94 v513)
      };
  
  moveDetailsFor v187 v188
    = concatMap2^movesForPiece v187 v188 (forcesColoured v187 v188);
  
  sum v98 = foldr^plus 0 v98;
  
  map^size v82 = case v82 of {
      Nil -> Nil;
      Cons v506 v507 ->
        Cons (size v506) (map^size v507)
      };
  
  snd v61 = case v61 of {
      Pair v494 v495 -> v495
      };
  
  unzip v99 = case v99 of {
      Nil -> Pair Nil Nil;
      Cons v514 v515 -> case v514 of {
          Pair v516 v517 -> let {
              v518 = unzip v515
              } in
              (Pair (Cons v516 (fst v518)) (Cons v517 (snd v518)))
          }
      };
  
  solnOr v288 v289 v290 v291
    = case v290 of {
      Pair v612 v613 -> case
          (replies v613 (opponent v288) ((-) v289 1))
        of {
          Nothing -> v291;
          Just v614 -> case (null v614) of
              {
              True -> case
                  (kingincheck (opponent v288) v613)
                of {
                  True ->
                    Just (Solution v612 Nil);
                  False -> v291
                  };
              False ->
                Just (Solution v612 v614)
              }
          }
      };
  
  concatMap2^movesForPiece v189 v190 v183
    = case v183 of {
      Empty -> Nil;
      Map v561 v562 v563 ->
        append (movesForPiece v189 v190 v561 v562) (concatMap2^movesForPiece v189 v190 v563)
      };
  
  forcesColoured v176 v177 = case
      v176 of {
      White -> case v177 of {
          Board v557 v558 -> v557
          };
      Black -> case v177 of {
          Board v559 v560 -> v560
          }
      };
  
  foldr^plus v94 v95 = case v95 of
      {
      Nil -> v94;
      Cons v512 v513 ->
        plus v512 (foldr^plus v94 v513)
      };
  
  fst v58 = case v58 of {
      Pair v492 v493 -> v492
      };
  
  replies v294 v295 v296 = let {
      v615 = moveDetailsFor v295 v294
      } in (case ((==) v296 0) of {
        True -> case (null v615) of {
            True -> Just Nil;
            False -> Nothing
            };
        False ->
          foldr^solnAnd v295 v296 (Just Nil) v615
        });
  
  opponent v152 = case v152 of {
      Black -> White;
      White -> Black
      };
  
  null v70 = case v70 of {
      Nil -> True;
      Cons v500 v501 -> False
      };
  
  kingincheck v254 v255
    = any2^kingInCheckFrom v254 v255 (forcesColoured (opponent v254) v255);
  
  append v73 v74 = case v73 of {
      Nil -> v74;
      Cons v502 v503 ->
        Cons v502 (append v503 v74)
      };
  
  movesForPiece v189 v190 v191 v192
    = concatMap^tryMove v189 v190 v191 v192 (rawmoves v189 v191 v192 v190);
  
  plus v44 v45 = (+) v44 v45;
  
  foldr^solnAnd v297 v298 v94 v95
    = case v95 of {
      Nil -> v94;
      Cons v512 v513 ->
        solnAnd v297 v298 v512 (foldr^solnAnd v297 v298 v94 v513)
      };
  
  any2^kingInCheckFrom v256 v257 v250
    = case v250 of {
      Empty -> False;
      Map v599 v600 v601 ->
        dis (kingInCheckFrom v256 v257 v599 v600) (any2^kingInCheckFrom v256 v257 v601)
      };
  
  concatMap^tryMove v193 v194 v195 v196 v86
    = case v86 of {
      Nil -> Nil;
      Cons v508 v509 ->
        append (tryMove v193 v194 v195 v196 v508) (concatMap^tryMove v193 v194 v195 v196 v509)
      };
  
  rawmoves v201 v202 v203 v204
    = (case v202 of {
      King -> kingmoves;
      Queen -> queenmoves;
      Rook -> rookmoves;
      Bishop -> bishopmoves;
      Knight -> knightmoves;
      Pawn -> pawnmoves
      }) v201 v203 v204;
  
  solnAnd v297 v298 v299 v300
    = case v299 of {
      Pair v616 v617 -> case
          (solution v617 (opponent v297) ((-) v298 1))
        of {
          Nothing -> Nothing;
          Just v618 -> case v300 of {
              Nothing -> Nothing;
              Just v619 ->
                Just (Cons (Pair v616 v618) v619)
              }
          }
      };
  
  dis v56 v57 = case v56 of {
      True -> True;
      False -> v57
      };
  
  kingInCheckFrom v256 v257 v258 v259
    = case v259 of {
      Pair v602 v603 -> case
          (kingSquare v256 v257) of {
          Pair v604 v605 -> case v258 of {
              King ->
                con ((<=) (abs ((-) v602 v604)) 1) ((<=) (abs ((-) v603 v605)) 1);
              Queen ->
                dis (kingInCheckFrom v256 v257 Rook (Pair v602 v603)) (kingInCheckFrom v256 v257 Bishop (Pair v602 v603));
              Rook ->
                dis (con ((==) v602 v604) (emptyAtAll v257 (filePath v604 v603 v605))) (con ((==) v603 v605) (emptyAtAll v257 (rankPath v605 v602 v604)));
              Bishop ->
                dis (con ((==) ((-) v602 v603) ((-) v604 v605)) (emptyAtAll v257 (diagPath^minus ((-) v604 v605) v602 v604))) (con ((==) ((+) v602 v603) ((+) v604 v605)) (emptyAtAll v257 (diagPath^plus ((+) v604 v605) v602 v604)));
              Knight ->
                dis (con ((==) (abs ((-) v602 v604)) 2) ((==) (abs ((-) v603 v605)) 1)) (con ((==) (abs ((-) v602 v604)) 1) ((==) (abs ((-) v603 v605)) 2));
              Pawn ->
                con ((==) (abs ((-) v602 v604)) 1) ((==) v605 (onFor v256 v603))
              }
          }
      };
  
  tryMove v193 v194 v195 v196 v197
    = case v197 of {
      Move v564 v565 v566 -> let {
          v567 = Pair v193 v195;
          v568 = rmPieceAt v193 v196 v194;
          v569 = maybe v567 id v566;
          v570 = maybe (putPieceAt v564 v569 v568) (const (putPieceAt v564 v569 (rmPieceAt (opponent v193) v564 v568))) v565
          } in (case
            (kingincheck v193 v570) of {
            False ->
              Cons (Pair (MoveInFull v567 v196 (Move v564 v565 v566)) v570) Nil;
            True -> Nil
            })
      };
  
  kingmoves v215 v216 v217 = case
      v216 of {
      Pair v573 v574 -> let {
          v575 = (+) v573 1;
          v576 = (-) v573 1;
          v577 = (+) v574 1;
          v578 = (-) v574 1
          } in
          (sift v215 v217 Nil (Cons (Pair v576 v577) (Cons (Pair v573 v577) (Cons (Pair v575 v577) (Cons (Pair v576 v574) (Cons (Pair v575 v574) (Cons (Pair v576 v578) (Cons (Pair v573 v578) (Cons (Pair v575 v578) Nil)))))))))
      };
  
  queenmoves v246 v247 v248
    = append (bishopmoves v246 v247 v248) (rookmoves v246 v247 v248);
  
  rookmoves v208 v209 v210
    = append (moveLine v210 v208 v209 (cross (Pair dec id))) (append (moveLine v210 v208 v209 (cross (Pair inc id))) (append (moveLine v210 v208 v209 (cross (Pair id dec))) (moveLine v210 v208 v209 (cross (Pair id inc)))));
  
  bishopmoves v205 v206 v207
    = append (moveLine v207 v205 v206 (cross (Pair dec inc))) (append (moveLine v207 v205 v206 (cross (Pair inc inc))) (append (moveLine v207 v205 v206 (cross (Pair dec dec))) (moveLine v207 v205 v206 (cross (Pair inc dec)))));
  
  knightmoves v220 v221 v222
    = case v221 of {
      Pair v579 v580 -> let {
          v581 = (+) v579 1;
          v582 = (-) v579 1;
          v583 = (+) v580 1;
          v584 = (-) v580 1;
          v585 = (+) v579 2;
          v586 = (-) v579 2;
          v587 = (+) v580 2;
          v588 = (-) v580 2
          } in
          (sift v220 v222 Nil (Cons (Pair v582 v587) (Cons (Pair v581 v587) (Cons (Pair v586 v583) (Cons (Pair v585 v583) (Cons (Pair v586 v584) (Cons (Pair v585 v584) (Cons (Pair v582 v588) (Cons (Pair v581 v588) Nil)))))))))
      };
  
  pawnmoves v231 v232 v233 = case
      v232 of {
      Pair v592 v593 -> let {
          v594 = case v231 of {
            White -> 1;
            Black -> (-) 0 1
            };
          v595 = Pair v592 ((+) v593 v594);
          v596 = Pair v592 ((+) ((+) v593 v594) v594)
          } in (append (case
            (no (pieceAt v233 v595)) of {
            True ->
              append (promote v231 v595 Nothing) (case
                (con (secondRank v231 v593) (no (pieceAt v233 v596)))
              of {
                True ->
                  Cons (Move v596 Nothing Nothing) Nil;
                False -> Nil
                });
            False -> Nil
            }) (append (promoteCap v231 (Pair ((+) v592 1) ((+) v593 v594)) v233) (promoteCap v231 (Pair ((-) v592 1) ((+) v593 v594)) v233)))
      };
  
  kingSquare v146 v147
    = kSq (forcesColoured v146 v147);
  
  con v54 v55 = case v54 of {
      True -> v55;
      False -> False
      };
  
  abs v43 = case ((<=) 0 v43) of {
      True -> v43;
      False -> (-) 0 v43
      };
  
  emptyAtAll v117 v118 = case v117
    of {
      Board v524 v525 ->
        emptyAtAllAnd v118 (emptyAtAllAnd v118 True v525) v524
      };
  
  filePath v263 v264 v265 v266
    = case v266 of {
      Pair v606 v607 ->
        con ((==) v606 v263) (con ((<=) ((+) (min v264 v265) 1) v607) ((<=) v607 ((-) (max v264 v265) 1)))
      };
  
  rankPath v269 v270 v271 v272
    = case v272 of {
      Pair v608 v609 ->
        con ((==) v609 v269) (con ((<=) ((+) (min v270 v271) 1) v608) ((<=) v608 ((-) (max v270 v271) 1)))
      };
  
  diagPath^minus v276 v277 v278 v279
    = case v279 of {
      Pair v610 v611 ->
        con ((==) (minus v610 v611) v276) (con ((<=) ((+) (min v277 v278) 1) v610) ((<=) v610 ((-) (max v277 v278) 1)))
      };
  
  diagPath^plus v276 v277 v278 v279
    = case v279 of {
      Pair v610 v611 ->
        con ((==) (plus v610 v611) v276) (con ((<=) ((+) (min v277 v278) 1) v610) ((<=) v610 ((-) (max v277 v278) 1)))
      };
  
  onFor v262 = case v262 of {
      Black -> inc;
      White -> dec
      };
  
  rmPieceAt v127 v128 v129 = case
      v127 of {
      White -> case v129 of {
          Board v529 v530 ->
            Board (rPa v128 v529) v530
          };
      Black -> case v129 of {
          Board v531 v532 ->
            Board v531 (rPa v128 v532)
          }
      };
  
  maybe v50 v51 v52 = case v52 of
      {
      Nothing -> v50;
      Just v491 -> v51 v491
      };
  
  id v34 = v34;
  
  putPieceAt v139 v140 v141 = case
      v140 of {
      Pair v536 v537 -> case v141 of {
          Board v538 v539 -> case v536 of
              {
              White ->
                Board (Map v537 v139 v538) v539;
              Black ->
                Board v538 (Map v537 v139 v539)
              }
          }
      };
  
  const v35 v36 = v35;
  
  sift v225 v226 v227 v228 = case
      v228 of {
      Nil -> v227;
      Cons v589 v590 -> case
          (onboard v589) of {
          False ->
            sift v225 v226 v227 v590;
          True -> case (pieceAt v226 v589)
            of {
              Nothing ->
                sift v225 v226 (Cons (Move v589 Nothing Nothing) v227) v590;
              Just v591 -> case
                  (sameColour (colourOf v591) v225)
                of {
                  True ->
                    sift v225 v226 v227 v590;
                  False ->
                    sift v225 v226 (Cons (Move v589 (Just v591) Nothing) v227) v590
                  }
              }
          }
      };
  
  moveLine v211 v212 v213 v214
    = let {
      v572 = v214 v213
      } in (case (onboard v572) of {
        True -> case (pieceAt v211 v572)
          of {
            Nothing ->
              Cons (Move v572 Nothing Nothing) (moveLine v211 v212 v572 v214);
            Just v571 -> case
                (sameColour (colourOf v571) v212)
              of {
                False ->
                  Cons (Move v572 (Just v571) Nothing) Nil;
                True -> Nil
                }
            };
        False -> Nil
        });
  
  cross v64 v65 = case v64 of {
      Pair v496 v497 -> case v65 of {
          Pair v498 v499 ->
            Pair (v496 v498) (v497 v499)
          }
      };
  
  dec v38 = (-) v38 1;
  
  inc v37 = (+) v37 1;
  
  no v48 = case v48 of {
      Nothing -> True;
      Just v490 -> False
      };
  
  pieceAt v106 v107 = case v106 of
      {
      Board v519 v520 ->
        pieceAtWith v107 White (pieceAtWith v107 Black Nothing v520) v519
      };
  
  promote v239 v240 v241 = case
      (lastRank v239 (rank v240)) of {
      True ->
        map^Move v240 v241 (Cons (Just (Pair v239 Queen)) (Cons (Just (Pair v239 Rook)) (Cons (Just (Pair v239 Bishop)) (Cons (Just (Pair v239 Knight)) Nil))));
      False ->
        Cons (Move v240 v241 Nothing) Nil
      };
  
  secondRank v242 v243 = case v242
    of {
      White -> (==) v243 2;
      Black -> (==) v243 7
      };
  
  promoteCap v236 v237 v238 = let
      {
      v598 = pieceAt v238 v237
      } in (case v598 of {
        Nothing -> Nil;
        Just v597 -> case
            (sameColour (colourOf v597) v236)
          of {
            False -> promote v236 v237 v598;
            True -> Nil
            }
        });
  
  kSq v148 = case v148 of {
      Map v540 v541 v542 -> case
          (isKing v540) of {
          True -> v541;
          False -> kSq v542
          }
      };
  
  emptyAtAllAnd v121 v122 v123
    = case v123 of {
      Empty -> v122;
      Map v526 v527 v528 -> case
          (v121 v527) of {
          True -> False;
          False ->
            emptyAtAllAnd v121 v122 v528
          }
      };
  
  min v39 v40 = case
      ((<=) v39 v40) of {
      True -> v39;
      False -> v40
      };
  
  max v41 v42 = case
      ((<=) v41 v42) of {
      True -> v42;
      False -> v41
      };
  
  minus v46 v47 = (-) v46 v47;
  
  rPa v134 v135 = case v135 of {
      Map v533 v534 v535 -> case
          (sameSquare v534 v134) of {
          True -> v535;
          False ->
            Map v533 v534 (rPa v134 v535)
          }
      };
  
  onboard v173 = case v173 of {
      Pair v555 v556 ->
        con (con ((<=) 1 v555) ((<=) v555 8)) (con ((<=) 1 v556) ((<=) v556 8))
      };
  
  sameColour v159 v160 = case v159
    of {
      White -> case v160 of {
          White -> True;
          Black -> False
          };
      Black -> case v160 of {
          White -> False;
          Black -> True
          }
      };
  
  colourOf v153 = case v153 of {
      Pair v543 v544 -> v543
      };
  
  pieceAtWith v110 v111 v112 v113
    = case v113 of {
      Empty -> v112;
      Map v521 v522 v523 -> case
          (sameSquare v522 v110) of {
          True -> Just (Pair v111 v521);
          False ->
            pieceAtWith v110 v111 v112 v523
          }
      };
  
  lastRank v244 v245 = case v244
    of {
      White -> (==) v245 8;
      Black -> (==) v245 1
      };
  
  rank v161 = case v161 of {
      Pair v547 v548 -> v548
      };
  
  map^Move v1 v2 v82 = case v82 of
      {
      Nil -> Nil;
      Cons v506 v507 ->
        Cons (Move v1 v2 v506) (map^Move v1 v2 v507)
      };
  
  isKing v105
    = (==) (kindToChar v105) 75;
  
  sameSquare v167 v168 = case v167
    of {
      Pair v551 v552 -> case v168 of {
          Pair v553 v554 ->
            con ((==) v551 v553) ((==) v552 v554)
          }
      };
  
  kindToChar v104 = case v104 of {
      King -> 75;
      Queen -> 81;
      Rook -> 82;
      Bishop -> 66;
      Knight -> 78;
      Pawn -> 80
      }
}
