{
  main 
    = solveProblem (Pair (Board (Cons (Pair Knight (Pair 7 8)) (Cons (Pair Rook (Pair 5 7)) (Cons (Pair King (Pair 8 7)) (Cons (Pair Bishop (Pair 4 5)) (Cons (Pair Pawn (Pair 8 4)) (Cons (Pair Pawn (Pair 7 3)) (Cons (Pair Pawn (Pair 5 2)) (Cons (Pair Pawn (Pair 6 2)) (Cons (Pair Queen (Pair 5 1)) Nil))))))))) (Cons (Pair Knight (Pair 2 8)) (Cons (Pair Pawn (Pair 7 7)) (Cons (Pair Pawn (Pair 4 6)) (Cons (Pair Pawn (Pair 3 5)) (Cons (Pair King (Pair 6 5)) (Cons (Pair Pawn (Pair 8 5)) (Cons (Pair Pawn (Pair 4 4)) (Cons (Pair Pawn (Pair 2 3)) (Cons (Pair Pawn (Pair 5 3)) (Cons (Pair Pawn (Pair 7 2)) (Cons (Pair Queen (Pair 1 1)) (Cons (Pair Knight (Pair 2 1)) (Cons (Pair Bishop (Pair 8 1)) Nil)))))))))))))) (Pair White 3));
  
  solveProblem v308 = case v308 of
      {
      Pair v635 v636 -> case v636 of {
          Pair v637 v638 ->
            solve v635 v637 v638
          }
      };
  
  solve v282 v283 v284
    = showResult (solution v282 v283 ((-) ((+) v284 v284) 1));
  
  showResult v303 = case v303 of {
      Nothing -> 0;
      Just v632 -> size v632
      };
  
  solution v285 v286 v287
    = foldr^solnOr v286 v287 Nothing (moveDetailsFor v286 v285);
  
  size v305 = case v305 of {
      Solution v633 v634 ->
        (+) 1 (sum (map^size (snd (unzip v634))))
      };
  
  foldr^solnOr v288 v289 v96 v97
    = case v97 of {
      Nil -> v96;
      Cons v518 v519 ->
        solnOr v288 v289 v518 (foldr^solnOr v288 v289 v96 v519)
      };
  
  moveDetailsFor v190 v191
    = concatMap^movesForPiece v190 v191 (forcesColoured v190 v191);
  
  sum v104 = sumAcc 0 v104;
  
  map^size v84 = case v84 of {
      Nil -> Nil;
      Cons v512 v513 ->
        Cons (size v512) (map^size v513)
      };
  
  snd v63 = case v63 of {
      Pair v500 v501 -> v501
      };
  
  unzip v105 = case v105 of {
      Nil -> Pair Nil Nil;
      Cons v522 v523 -> case v522 of {
          Pair v524 v525 -> let {
              v526 = unzip v523
              } in
              (Pair (Cons v524 (fst v526)) (Cons v525 (snd v526)))
          }
      };
  
  solnOr v288 v289 v290 v291
    = case v290 of {
      Pair v624 v625 -> case
          (replies v625 (opponent v288) ((-) v289 1))
        of {
          Nothing -> v291;
          Just v626 -> case (null v626) of
              {
              True -> case
                  (kingincheck (opponent v288) v625)
                of {
                  True ->
                    Just (Solution v624 Nil);
                  False -> v291
                  };
              False ->
                Just (Solution v624 v626)
              }
          }
      };
  
  concatMap^movesForPiece v192 v193 v88
    = case v88 of {
      Nil -> Nil;
      Cons v514 v515 ->
        append (movesForPiece v192 v193 v514) (concatMap^movesForPiece v192 v193 v515)
      };
  
  forcesColoured v184 v185 = case
      v184 of {
      White -> case v185 of {
          Board v569 v570 -> v569
          };
      Black -> case v185 of {
          Board v571 v572 -> v572
          }
      };
  
  sumAcc v100 v101 = case v101 of
      {
      Nil -> v100;
      Cons v520 v521 ->
        sumAcc ((+) v100 v520) v521
      };
  
  fst v60 = case v60 of {
      Pair v498 v499 -> v498
      };
  
  replies v294 v295 v296 = let {
      v627 = moveDetailsFor v295 v294
      } in (case ((==) v296 0) of {
        True -> case (null v627) of {
            True -> Just Nil;
            False -> Nothing
            };
        False ->
          foldr^solnAnd v295 v296 (Just Nil) v627
        });
  
  opponent v160 = case v160 of {
      Black -> White;
      White -> Black
      };
  
  null v72 = case v72 of {
      Nil -> True;
      Cons v506 v507 -> False
      };
  
  kingincheck v253 v254
    = any^kingInCheckFrom v253 v254 (forcesColoured (opponent v253) v254);
  
  append v75 v76 = case v75 of {
      Nil -> v76;
      Cons v508 v509 ->
        Cons v508 (append v509 v76)
      };
  
  movesForPiece v192 v193 v194
    = concatMap^tryMove v192 v193 v194 (rawmoves v192 v194 v193);
  
  foldr^solnAnd v297 v298 v96 v97
    = case v97 of {
      Nil -> v96;
      Cons v518 v519 ->
        solnAnd v297 v298 v518 (foldr^solnAnd v297 v298 v96 v519)
      };
  
  any^kingInCheckFrom v255 v256 v92
    = case v92 of {
      Nil -> False;
      Cons v516 v517 ->
        dis (kingInCheckFrom v255 v256 v516) (any^kingInCheckFrom v255 v256 v517)
      };
  
  concatMap^tryMove v195 v196 v197 v88
    = case v88 of {
      Nil -> Nil;
      Cons v514 v515 ->
        append (tryMove v195 v196 v197 v514) (concatMap^tryMove v195 v196 v197 v515)
      };
  
  rawmoves v204 v205 v206 = case
      v205 of {
      Pair v582 v583 -> (case v582 of
          {
          King -> kingmoves;
          Queen -> queenmoves;
          Rook -> rookmoves;
          Bishop -> bishopmoves;
          Knight -> knightmoves;
          Pawn -> pawnmoves
          }) v204 v583 v206
      };
  
  solnAnd v297 v298 v299 v300
    = case v299 of {
      Pair v628 v629 -> case
          (solution v629 (opponent v297) ((-) v298 1))
        of {
          Nothing -> Nothing;
          Just v630 -> case v300 of {
              Nothing -> Nothing;
              Just v631 ->
                Just (Cons (Pair v628 v630) v631)
              }
          }
      };
  
  dis v58 v59 = case v58 of {
      True -> True;
      False -> v59
      };
  
  kingInCheckFrom v255 v256 v257
    = case v257 of {
      Pair v612 v613 -> case v613 of {
          Pair v614 v615 -> case
              (kingSquare v255 v256) of {
              Pair v616 v617 -> case v612 of {
                  King ->
                    con ((<=) (abs ((-) v614 v616)) 1) ((<=) (abs ((-) v615 v617)) 1);
                  Queen ->
                    dis (kingInCheckFrom v255 v256 (Pair Rook (Pair v614 v615))) (kingInCheckFrom v255 v256 (Pair Bishop (Pair v614 v615)));
                  Rook ->
                    dis (con ((==) v614 v616) (emptyAtAll v256 (filePath v616 v615 v617))) (con ((==) v615 v617) (emptyAtAll v256 (rankPath v617 v614 v616)));
                  Bishop ->
                    dis (con ((==) ((-) v614 v615) ((-) v616 v617)) (emptyAtAll v256 (diagPath^minus ((-) v616 v617) v614 v616))) (con ((==) ((+) v614 v615) ((+) v616 v617)) (emptyAtAll v256 (diagPath^plus ((+) v616 v617) v614 v616)));
                  Knight ->
                    dis (con ((==) (abs ((-) v614 v616)) 2) ((==) (abs ((-) v615 v617)) 1)) (con ((==) (abs ((-) v614 v616)) 1) ((==) (abs ((-) v615 v617)) 2));
                  Pawn ->
                    con ((==) (abs ((-) v614 v616)) 1) ((==) v617 (onFor v255 v615))
                  }
              }
          }
      };
  
  tryMove v195 v196 v197 v198
    = case v197 of {
      Pair v573 v574 -> case v198 of {
          Move v575 v576 v577 -> let {
              v578 = Pair v195 v573;
              v579 = rmPieceAt v195 v574 v196;
              v580 = maybe v578 id v577;
              v581 = maybe (putPieceAt v575 v580 v579) (const (putPieceAt v575 v580 (rmPieceAt (opponent v195) v575 v579))) v576
              } in (case
                (kingincheck v195 v581) of {
                False ->
                  Cons (Pair (MoveInFull v578 v574 (Move v575 v576 v577)) v581) Nil;
                True -> Nil
                })
          }
      };
  
  kingmoves v219 v220 v221 = case
      v220 of {
      Pair v586 v587 -> let {
          v588 = (+) v586 1;
          v589 = (-) v586 1;
          v590 = (+) v587 1;
          v591 = (-) v587 1
          } in
          (sift v219 v221 Nil (Cons (Pair v589 v590) (Cons (Pair v586 v590) (Cons (Pair v588 v590) (Cons (Pair v589 v587) (Cons (Pair v588 v587) (Cons (Pair v589 v591) (Cons (Pair v586 v591) (Cons (Pair v588 v591) Nil)))))))))
      };
  
  queenmoves v250 v251 v252
    = append (bishopmoves v250 v251 v252) (rookmoves v250 v251 v252);
  
  rookmoves v212 v213 v214
    = append (moveLine v214 v212 v213 (cross (Pair dec id))) (append (moveLine v214 v212 v213 (cross (Pair inc id))) (append (moveLine v214 v212 v213 (cross (Pair id dec))) (moveLine v214 v212 v213 (cross (Pair id inc)))));
  
  bishopmoves v209 v210 v211
    = append (moveLine v211 v209 v210 (cross (Pair dec inc))) (append (moveLine v211 v209 v210 (cross (Pair inc inc))) (append (moveLine v211 v209 v210 (cross (Pair dec dec))) (moveLine v211 v209 v210 (cross (Pair inc dec)))));
  
  knightmoves v224 v225 v226
    = case v225 of {
      Pair v592 v593 -> let {
          v594 = (+) v592 1;
          v595 = (-) v592 1;
          v596 = (+) v593 1;
          v597 = (-) v593 1;
          v598 = (+) v592 2;
          v599 = (-) v592 2;
          v600 = (+) v593 2;
          v601 = (-) v593 2
          } in
          (sift v224 v226 Nil (Cons (Pair v595 v600) (Cons (Pair v594 v600) (Cons (Pair v599 v596) (Cons (Pair v598 v596) (Cons (Pair v599 v597) (Cons (Pair v598 v597) (Cons (Pair v595 v601) (Cons (Pair v594 v601) Nil)))))))))
      };
  
  pawnmoves v235 v236 v237 = case
      v236 of {
      Pair v605 v606 -> let {
          v607 = case v235 of {
            White -> 1;
            Black -> (-) 0 1
            };
          v608 = Pair v605 ((+) v606 v607);
          v609 = Pair v605 ((+) ((+) v606 v607) v607)
          } in (append (case
            (no (pieceAt v237 v608)) of {
            True ->
              append (promote v235 v608 Nothing) (case
                (con (secondRank v235 v606) (no (pieceAt v237 v609)))
              of {
                True ->
                  Cons (Move v609 Nothing Nothing) Nil;
                False -> Nil
                });
            False -> Nil
            }) (append (promoteCap v235 (Pair ((+) v605 1) ((+) v606 v607)) v237) (promoteCap v235 (Pair ((-) v605 1) ((+) v606 v607)) v237)))
      };
  
  kingSquare v153 v154
    = kSq (forcesColoured v153 v154);
  
  con v56 v57 = case v56 of {
      True -> v57;
      False -> False
      };
  
  abs v45 = case ((<=) 0 v45) of {
      True -> v45;
      False -> (-) 0 v45
      };
  
  emptyAtAll v124 v125 = case v124
    of {
      Board v533 v534 ->
        emptyAtAllAnd v125 (emptyAtAllAnd v125 True v534) v533
      };
  
  filePath v263 v264 v265 v266
    = case v266 of {
      Pair v618 v619 ->
        con ((==) v618 v263) (con ((<=) ((+) (min v264 v265) 1) v619) ((<=) v619 ((-) (max v264 v265) 1)))
      };
  
  rankPath v269 v270 v271 v272
    = case v272 of {
      Pair v620 v621 ->
        con ((==) v621 v269) (con ((<=) ((+) (min v270 v271) 1) v620) ((<=) v620 ((-) (max v270 v271) 1)))
      };
  
  diagPath^minus v276 v277 v278 v279
    = case v279 of {
      Pair v622 v623 ->
        con ((==) (minus v622 v623) v276) (con ((<=) ((+) (min v277 v278) 1) v622) ((<=) v622 ((-) (max v277 v278) 1)))
      };
  
  diagPath^plus v276 v277 v278 v279
    = case v279 of {
      Pair v622 v623 ->
        con ((==) (plus v622 v623) v276) (con ((<=) ((+) (min v277 v278) 1) v622) ((<=) v622 ((-) (max v277 v278) 1)))
      };
  
  onFor v262 = case v262 of {
      Black -> inc;
      White -> dec
      };
  
  rmPieceAt v135 v136 v137 = case
      v135 of {
      White -> case v137 of {
          Board v539 v540 ->
            Board (rPa v136 v539) v540
          };
      Black -> case v137 of {
          Board v541 v542 ->
            Board v541 (rPa v136 v542)
          }
      };
  
  maybe v52 v53 v54 = case v54 of
      {
      Nothing -> v52;
      Just v497 -> v53 v497
      };
  
  id v36 = v36;
  
  putPieceAt v146 v147 v148 = case
      v147 of {
      Pair v547 v548 -> case v148 of {
          Board v549 v550 -> case v547 of
              {
              White ->
                Board (Cons (Pair v548 v146) v549) v550;
              Black ->
                Board v549 (Cons (Pair v548 v146) v550)
              }
          }
      };
  
  const v37 v38 = v37;
  
  sift v229 v230 v231 v232 = case
      v232 of {
      Nil -> v231;
      Cons v602 v603 -> case
          (onboard v602) of {
          False ->
            sift v229 v230 v231 v603;
          True -> case (pieceAt v230 v602)
            of {
              Nothing ->
                sift v229 v230 (Cons (Move v602 Nothing Nothing) v231) v603;
              Just v604 -> case
                  (sameColour (colourOf v604) v229)
                of {
                  True ->
                    sift v229 v230 v231 v603;
                  False ->
                    sift v229 v230 (Cons (Move v602 (Just v604) Nothing) v231) v603
                  }
              }
          }
      };
  
  moveLine v215 v216 v217 v218
    = let {
      v585 = v218 v217
      } in (case (onboard v585) of {
        True -> case (pieceAt v215 v585)
          of {
            Nothing ->
              Cons (Move v585 Nothing Nothing) (moveLine v215 v216 v585 v218);
            Just v584 -> case
                (sameColour (colourOf v584) v216)
              of {
                False ->
                  Cons (Move v585 (Just v584) Nothing) Nil;
                True -> Nil
                }
            };
        False -> Nil
        });
  
  cross v66 v67 = case v66 of {
      Pair v502 v503 -> case v67 of {
          Pair v504 v505 ->
            Pair (v502 v504) (v503 v505)
          }
      };
  
  dec v40 = (-) v40 1;
  
  inc v39 = (+) v39 1;
  
  no v50 = case v50 of {
      Nothing -> True;
      Just v496 -> False
      };
  
  pieceAt v112 v113 = case v112 of
      {
      Board v527 v528 ->
        pieceAtWith v113 White (pieceAtWith v113 Black Nothing v528) v527
      };
  
  promote v243 v244 v245 = case
      (lastRank v243 (rank v244)) of {
      True ->
        map^Move v244 v245 (Cons (Just (Pair v243 Queen)) (Cons (Just (Pair v243 Rook)) (Cons (Just (Pair v243 Bishop)) (Cons (Just (Pair v243 Knight)) Nil))));
      False ->
        Cons (Move v244 v245 Nothing) Nil
      };
  
  secondRank v246 v247 = case v246
    of {
      White -> (==) v247 2;
      Black -> (==) v247 7
      };
  
  promoteCap v240 v241 v242 = let
      {
      v611 = pieceAt v242 v241
      } in (case v611 of {
        Nothing -> Nil;
        Just v610 -> case
            (sameColour (colourOf v610) v240)
          of {
            False -> promote v240 v241 v611;
            True -> Nil
            }
        });
  
  kSq v155 = case v155 of {
      Cons v551 v552 -> case v551 of {
          Pair v553 v554 -> case
              (isKing v553) of {
              True -> v554;
              False -> kSq v552
              }
          }
      };
  
  emptyAtAllAnd v128 v129 v130
    = case v130 of {
      Nil -> v129;
      Cons v535 v536 -> case v535 of {
          Pair v537 v538 -> case
              (v128 v538) of {
              True -> False;
              False ->
                emptyAtAllAnd v128 v129 v536
              }
          }
      };
  
  min v41 v42 = case
      ((<=) v41 v42) of {
      True -> v41;
      False -> v42
      };
  
  max v43 v44 = case
      ((<=) v43 v44) of {
      True -> v44;
      False -> v43
      };
  
  minus v48 v49 = (-) v48 v49;
  
  plus v46 v47 = (+) v46 v47;
  
  rPa v142 v143 = case v143 of {
      Cons v543 v544 -> case v543 of {
          Pair v545 v546 -> case
              (sameSquare v546 v142) of {
              True -> v544;
              False ->
                Cons v543 (rPa v142 v544)
              }
          }
      };
  
  onboard v181 = case v181 of {
      Pair v567 v568 ->
        con (con ((<=) 1 v567) ((<=) v567 8)) (con ((<=) 1 v568) ((<=) v568 8))
      };
  
  sameColour v167 v168 = case v167
    of {
      White -> case v168 of {
          White -> True;
          Black -> False
          };
      Black -> case v168 of {
          White -> False;
          Black -> True
          }
      };
  
  colourOf v161 = case v161 of {
      Pair v555 v556 -> v555
      };
  
  pieceAtWith v116 v117 v118 v119
    = case v119 of {
      Nil -> v118;
      Cons v529 v530 -> case v529 of {
          Pair v531 v532 -> case
              (sameSquare v532 v116) of {
              True -> Just (Pair v117 v531);
              False ->
                pieceAtWith v116 v117 v118 v530
              }
          }
      };
  
  lastRank v248 v249 = case v248
    of {
      White -> (==) v249 8;
      Black -> (==) v249 1
      };
  
  rank v169 = case v169 of {
      Pair v559 v560 -> v560
      };
  
  map^Move v1 v2 v84 = case v84 of
      {
      Nil -> Nil;
      Cons v512 v513 ->
        Cons (Move v1 v2 v512) (map^Move v1 v2 v513)
      };
  
  isKing v111
    = (==) (kindToChar v111) 75;
  
  sameSquare v175 v176 = case v175
    of {
      Pair v563 v564 -> case v176 of {
          Pair v565 v566 ->
            con ((==) v563 v565) ((==) v564 v566)
          }
      };
  
  kindToChar v110 = case v110 of {
      King -> 75;
      Queen -> 81;
      Rook -> 82;
      Bishop -> 66;
      Knight -> 78;
      Pawn -> 80
      }
}
