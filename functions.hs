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
my_filter p l = 
    if length l == 0 then []
    else (if p (head l) then [head l] else []) ++ my_filter p (tail l)

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

cat a b c
    | length a == 0 = []
    | length b == 0 = []
    | length c == 0 = []
    | otherwise = [(head a) ++ " " ++ (take 1 (head b)) ++ ". " ++ (head c)]

is_even :: Int -> Bool
is_even x = (modulus x 2) == 0 

wholeName a b c = 
    if is_even (length (head (cat a b c))) then cat a b c ++ wholeName (tail a) (tail b) (tail c) else []