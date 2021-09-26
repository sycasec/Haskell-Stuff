cube :: Int -> Int
cube x = x * x * x

double :: Int -> Int
double x = 2 * x

modulus :: Int -> Int -> Int
modulus x y
    | y > x = x
    | x - y >= y = modulus(x - y) y
    | otherwise = x - y

factorial :: Int -> Int
factorial x = if (x == 0) then 1 else (x * factorial (x - 1))

summation :: Int -> Int
summation x = if (x <= 1) then x else (x + (summation (x - 1)))

compose :: (Int -> Int) -> (Int -> Int) -> (Int -> Int)
compose f g = \x -> f (g x)

subtractMaker :: Int -> (Int -> Int)
subtractMaker x = (\y -> y - x)

applyNTimes :: (Int -> Int) -> Int -> Int -> Int
applyNTimes f 1 x = f x
applyNTimes f n x = f (applyNTimes f (n-1) x)

my_map :: (a -> b) -> [a] -> [b]
my_map f l =
    if length l == 0 then []
    else [f (head l)] ++ my_map f (tail l)

my_filter :: (a -> Bool) -> [a] -> [a]
my_filter _ [] = []
my_filter f (x:xs)
  | f x = x : (my_filter f xs)
  | otherwise = my_filter f xs

my_foldl :: (a -> a -> a) -> a -> [a] -> a
my_foldl f u l =
    if length l == 0 then u
    else my_foldl f (f u (head l)) (tail l)

my_foldr :: (a -> a -> a) -> a -> [a] -> a
my_foldr f u l =
    if length l == 0 then u
    else f (head l) (my_foldr f u (tail l))

my_zip :: (a -> b -> c) -> [a] -> [b] -> [c]
my_zip f l m
    | length l == 0 = []
    | length m == 0 = []
    | otherwise = ([f(head l) (head m)]) ++ my_zip f (tail l) (tail m)

sum_of_squares :: (Num a) => [a] -> a
sum_of_squares l
    | length l == 0 = (head l)
    | otherwise = sum (my_map (\x -> x * x) l)

modulus :: Int -> Int -> Int
modulus x y
    | y > x = x
    | x - y >= y = modulus(x - y) y
    | otherwise = x - y

is_even :: Int -> Bool
is_even x = (modulus x 2) == 0

even_name :: [a] -> Bool
even_name x =
    is_even (length x)

wholeName :: [[Char]] -> [[Char]] -> [[Char]] -> [[Char]]
wholeName a b c =
    do my_filter even_name x where x = my_zip (\x y -> x ++ " " ++ y) (my_zip (\x y -> x ++ " " ++ (take 1 y) ++ ".") (a) (b)) (c)
