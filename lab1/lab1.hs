a :: Int -> Int -> Int
a x y = x + y

b :: Int -> String
b x = if even x then "par" else "impar"

c :: Int  -> Int
c 1 = 1
c x = x * c (x - 1)

d :: Int -> Int -> Bool
d x y = x > 2 * y