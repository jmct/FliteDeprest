{

min x y = case (<=) x y of
 {
  False -> y;
  True  -> x
 } ;

main =   min  5 5 ; 

}



