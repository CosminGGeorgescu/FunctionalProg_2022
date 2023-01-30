factori :: Int -> [Int]
factori n = [x | x <- [1..n], n `mod` x == 0]

prim :: Int -> Bool
prim n = length (factori n) == 2

numerePrime :: Int -> [Int]
numerePrime n = [x | x <- [2..n], prim x]

myzip3 :: [Int] -> [Int] -> [Int] -> [(Int, Int, Int)]
myzip3 (a : as) (b : bs) (c : cs) = (a, b, c) : myzip3 as bs cs
myzip3 _ _ _ = []

firstEl :: [(a, b)] -> [a]
firstEl [] = []
firstEl ((x1, x2) : xs) = x1 : firstEl xs

sumList :: [[Int]] -> [Int]
sumList l = map sum l

prel2 :: [Int] -> [Int]
prel2 l = map (\x -> if even x then x `div` 2 else x * 2) l

f8 :: Char -> [String] -> [String]
f8 c l = filter (elem c) l

f9 :: [Int] -> [Int]
f9 l = map (^ 2) (filter odd l)

f10 :: [Int] -> [Int]
f10 l = map (\(x, y) -> y ^ 2) (filter (\(x , y) -> odd x) (zip [0..length l] l))

numaiVocale :: [String] -> [String]
numaiVocale l = map (\x -> filter (\y -> elem y "AEIOUaeiou") x) l

mymap :: (a -> b) -> [a] -> [b]
mymap _ [] = []
mymap f (x : xs) = (f x) : mymap f xs

myfilter :: (a -> Bool) -> [a] -> [a]
myfilter _ [] = []
myfilter f (x : xs) = if f x then x : myfilter f xs else myfilter f xs