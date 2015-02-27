{
  data Bool = True | False;
  data List a = Nil | Cons a (List a);

  and False y = False;
  and True  y = y;
  
  or True y = True;
  or False y = y;

  ifte p t f = case p of {
                True -> t;
                False -> f;
             };

  any p Nil         = False ;
  any p (Cons x xs) = or (p x) (any p xs) ;
  
  map f Nil = Nil;
  map f (Cons x xs) = Cons (f x) (map f xs);

  filter p Nil = Nil;
  filter p (Cons x xs) = ifte (p x) (Cons x (filter p xs)) (filter p xs);

  head (Cons x xs) = x;

  tail (Cons x xs) = xs;

  transpose Nil = Nil;
  transpose (Cons Nil xss) = transpose xss;
  transpose (Cons (Cons x xs) xss) = let {ys = Cons x (map head xss) } 
                                     in Cons ys (transpose (Cons xs (map tail xss)));

  diagonals (Cons r Nil) = map singleton r;
  diagonals (Cons r rs) = zipinit r (Cons Nil (diagonals rs));

  zipinit Nil ys = ys;
  zipinit (Cons x xs) (Cons y ys) = Cons (Cons x y) (zipinit xs ys);

  singleton x = Cons x Nil;

  contains xs ys = any (prefix xs) (suffixes ys);

  suffixes Nil = Nil;
  suffixes (Cons x xs) = Cons (Cons x xs) (suffixes xs);
  
  prefix Nil ys = True;
  prefix (Cons x xs) Nil = False;
  prefix (Cons x xs) (Cons y ys) = and ((==) x y) (prefix xs ys);

}
