# Equal
a;
a;

2 + 3;
\s. \z. s (s (s (s (s z))));

2 + 4;
3 + 3;

7 + 1;
2 * 4;

1 :: 2 :: 3 :: [];
tail 0 :: 1 :: 2 :: 3 :: [];

a;
(\x. x) a;

a a;
(\x. x x) a;

b;
((\f. \s. s) a) b;

fst 1, 2;
0 + 1;

(\x. \y. x);
(\a. \b. a);

\x. a;
(\f. \s. a) b;

(a b) (c d);
(a b) ((\x. x d) c);

# Not equal
a;
b;

a a;
a b;

1 :: [];
1;

2;
3;

\x. \y. x y;
\x. x;

\x. x;
x;

(\x. x);
(\x. x x);

10;
10, 0;

true;
false;
