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