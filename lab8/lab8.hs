data Punct = Pt [Int]

instance Show Punct where
    show (Pt l) = "(" ++ aux l ++ ")" where
        aux [x] = show x
        aux (x : xs) = show x ++ ", " ++ aux xs

data Arb = Vid | F Int | N Arb Arb
    deriving Show
class ToFromArb a where
    toArb :: a -> Arb
    fromArb :: Arb -> a

instance ToFromArb Punct where
    toArb (Pt []) = Vid
    toArb (Pt (x : xs)) = N (F x) (toArb (Pt xs))
    fromArb arb = Pt (aux arb) where
        aux Vid = []
        aux (F x) = [x]
        aux (N a1 a2) = aux a1 ++ aux a2

data Geo a = Square a | Rectangle a a | Circle a
    deriving Show

class GeoOps g where
    perimeter :: (Floating a) => g a -> a
    area :: (Floating a) =>  g a -> a

instance GeoOps Geo where
    perimeter (Square x) = 4 * x
    perimeter (Rectangle x y) = 2 * (x + y)
    perimeter (Circle x) = pi * x
    area (Square x) = x ^ 2
    area (Rectangle x y) = x * y
    area (Circle x) = pi * x ^ 2

instance (Eq a, Floating a) => Eq (Geo a) where
    g1 == g2 = perimeter g1 == perimeter g2
    g1 /= g2 = perimeter g1 /= perimeter g2