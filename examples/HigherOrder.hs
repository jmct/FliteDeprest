{
    inc x = (+) 1 x;

    add x y = (+) x y;

    map f Nil = Nil;
    map f (Cons x xs) = Cons (f x) (map f xs);

    list = Cons 1 (Cons 2 (Cons 3 Nil));

    main = map (add 1) list ;
}
