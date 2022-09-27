\y. (\x. \y. x) y;
\y. ((\x. y x) (\z. z)) y;

a :: [];

if (1 + 2) = 4 then 17 else head tail 1 :: 2 :: [];
1 + 1;
tail 1 :: 2 :: 3 :: [];

# Recursive list sum
(fix \f. \l. if isnil l then 0 else (head l) + (f (tail l))) (1 :: 2 :: 3 :: 4 :: []);

# Pairs
fst snd 1, (2, 3);

# Omega - should not loop
(\f. \s. s) ((\x. x x) (\x. x x));

# Terms with free variables
(\x. x c) a;
(c ((\t. x t) (\z. z b))) y;
((\t. x t) (\z. z b)) y;
(a b) c;
a ((\x. x) b);
