poly2 :: Double -> Double -> Double -> Double -> Double
poly2 a b c x = a * x ^ 2 + b * x + c

eeny :: Int -> String
eeny x = if even x then "eeny" else "meeny"

fizzbuzz1 :: Int -> String
fizzbuzz1 x = if x `mod` 15 == 0 then "Fizzbuzz" else if x `mod` 5 == 0 then "Buzz" else "Fizz"
fizzbuzz2 :: Int -> String
fizzbuzz2 x
    | x `mod` 15 == 0 = "Fizzbuzz"
    | x `mod` 5 == 0 = "Buzz"
    | x `mod` 3 == 0 = "Fizz"

tribonacci :: Int -> Int
tribonacci n
    | n == 1 || n == 2 = 1
    | n == 3 = 2
    | otherwise = tribonacci (n - 1) + tribonacci (n - 2) + tribonacci (n - 3)

binomial :: Int -> Int -> Int
binomial n k
    | k == 0 = 1
    | n == 0 = 0
    | otherwise = binomial (n - 1) k + binomial (n - 1) (k - 1)

verifL :: [Int] -> Bool
verifL l = even (length l)

takefinal :: [Int] -> Int -> [Int]
takefinal l n = reverse (take n (reverse l))

myreplicate :: Int -> a -> [a]
myreplicate 0 _ = []
myreplicate n v = [v] ++ myreplicate (n - 1) v

sumImp :: [Int] -> Int
sumImp [] = 0
sumImp (x : xs) = if odd x then x + sumImp xs else sumImp xs

totalLen :: [String] -> Int
totalLen [] = 0
totalLen (x : xs) = if take 1 x == "A" then length x + totalLen xs else totalLen xs