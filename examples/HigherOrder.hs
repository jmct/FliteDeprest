{
    inc x = (+) 1 x;

    map f Nil = Nil;
    map f (Cons x xs) = Cons (f x) (map f xs);

    list = Cons 1 (Cons 2 (Cons 3 Nil));

    main = map inc list;
}
