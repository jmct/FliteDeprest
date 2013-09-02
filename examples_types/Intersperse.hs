{
 data List a  = Nil | Cons a (List a); 
--
--intersperse i Nil                   =  Nil ;
--intersperse i (Cons x Nil)          =  Nil;--Cons x Nil ;
--intersperse i (Cons x (Cons y ys))  =  Cons x (Cons i (intersperse i (Cons y ys))) ;


inter i xs = case xs of
            { Nil -> Nil;
              Cons x (Cons y ys) -> Cons x (Cons i (inter i (Cons y ys))) ; 
             };



--
id i xs = case xs of
        { Nil -> Nil;
          Cons x (Cons y ys) -> Cons y (id y ys);
        };



main = 1;
}
