import Data.Char (isDigit, digitToInt)

nrVocale1 :: [String] -> Int
nrVocale1 [] = 0
nrVocale1 (x : xs) = if x == reverse x then sum (map (\x -> if elem x "aeiou" then 1 else 0) x) + nrVocale1 xs else nrVocale1 xs
nrVocale2 :: [String] -> Int
nrVocale2 l = sum (map (\x -> length (filter (\y -> elem y "aeiou") x)) (filter (\x -> x == reverse x && any (\y -> elem y "aeiou") x) l))
nrVocale3 :: [String] -> Int
nrVocale3 l = sum [length (filter (\y -> elem y "aeiou") x) | x <- l, x == reverse x, any (\y -> elem y "aeiou") x]

f :: Int -> [Int] -> [Int]
f _ [] = []
f n (x : xs) = if even x then [x, n] ++ f n xs else [x] ++ f n xs

divizori1 :: Int -> [Int]
divizori1 n = [x | x <- [1..n], n `mod` x == 0]
divizori2 :: Int -> [Int]
divizori2 n = filter (\x -> n `mod` x == 0) [1..n]

listadiv :: [Int] -> [[Int]]
listadiv [] = []
listadiv (x : xs) = [divizori1 x] ++ listadiv xs

inIntervalRec :: Int -> Int -> [Int] -> [Int]
inIntervalRec _ _ [] = []
inIntervalRec inf sup (x : xs) = if inf <= x && x <= sup then [x] ++ inIntervalRec inf sup xs else inIntervalRec inf sup xs
inIntervalComp :: Int -> Int -> [Int] -> [Int]
inIntervalComp inf sup l = [x | x <- l, inf <= x && x <= sup]
inInterval3 :: Int -> Int -> [Int] -> [Int]
inInterval3 inf sup l = filter (\x -> inf <= x && x <= sup) l

pozitiveRec :: [Int] -> Int
pozitiveRec [] = 0
pozitiveRec (x : xs) = if x > 0 then 1 + pozitiveRec xs else pozitiveRec xs
pozitiveComp :: [Int] -> Int
pozitiveComp l = length [x | x <- l, x > 0]
pozitive3 :: [Int] -> Int
pozitive3 l = foldr (\x y -> 1 + y) 0 (filter (> 0) l)

aux :: Int -> [Int] -> [Int]
aux _ [] = []
aux n (x : xs) = if odd x then [n] ++ aux (n + 1) xs else aux (n + 1) xs
pozitiiImpareRec :: [Int] -> [Int]
pozitiiImpareRec l = aux 0 l
pozitiiImpareComp :: [Int] -> [Int]
pozitiiImpareComp l = [x | (x, y) <- zip [0..length l] l, odd y]

multiDigitsRec :: String -> Int
multiDigitsRec [] = 1
multiDigitsRec (c : cs) = if isDigit c then digitToInt c * multiDigitsRec cs else multiDigitsRec cs
multiDigitsComp :: String -> Int
multiDigitsComp s = product [digitToInt x | x <- s, isDigit x]