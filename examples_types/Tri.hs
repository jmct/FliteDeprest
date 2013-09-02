{

 tri n = case (<=) n 1 of
 {
  False -> (+) (tri ((-) n 1)) n ;
  True  -> 1
 } ;

main =   tri 5 ; 

}

