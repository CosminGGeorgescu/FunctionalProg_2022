f1 :: [Int] -> Int
f1 l = foldr (\x y -> if odd x then y + x ^ 2 else y) 0 l

f2 :: [Bool] -> Bool
f2 l = foldr (&&) True l

allVerifies :: (Int -> Bool) -> [Int] -> Bool
allVerifies f l = f2 (map f l)

anyVerifies :: (Int -> Bool) -> [Int] -> Bool
anyVerifies f l = foldr (||) False (map f l)

mapFoldr :: (a -> b) -> [a] -> [b]
mapFoldr f l = foldr (\x y -> f x : y) [] l

filterFoldr :: (a -> Bool) -> [a] -> [a]
filterFoldr f l = foldr (\x y -> if f x then x : y else y) [] l

listToInt :: [Int] -> Int
listToInt l = foldl (\x y -> 10 * x + y) 0 l

rmChar1 :: Char -> String -> String
rmChar1 c s = foldr (\x y -> if x == c then y else x : y) [] s
rmChar2 :: Char -> String -> String
rmChar2 c s = foldl (\x y -> if y == c then x else x ++ [y]) [] s

rmCharRec :: String -> String -> String
rmCharRec s1 s2 = [x | x <- s2, length s1 == length (rmChar1 x s1)]

rmCharFold :: String -> String -> String
rmCharFold s1 s2 = foldr (\x y -> if elem x s1 then y else x : y) [] s2