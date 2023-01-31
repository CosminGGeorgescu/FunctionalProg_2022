import Data.Functor.Identity

data List a = Nil | Cons a (List a)
    deriving (Eq, Show)

instance Functor List where
    fmap f Nil = Nil
    fmap f (Cons a l) = Cons (f a) (fmap f l)

instance Applicative List where
    pure x = Cons x Nil
    Nil <*> _ = Nil
    _ <*> Nil = Nil
    (Cons a l1) <*> l2 = append (fmap a l2) (l1 <*> l2) where
        append Nil x = x
        append (Cons x l1) l2 = Cons x (append l1 l2)

data Cow = Cow {name :: String, age :: Int, weight :: Int}
    deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty s = Just s

noNegative :: Int -> Maybe Int
noNegative x = if x >= 0 then Just x else Nothing

cowFromString1 :: String -> Int -> Int -> Maybe Cow
cowFromString1 s x y
    | noEmpty s == Nothing = Nothing
    | noNegative x == Nothing = Nothing
    | noNegative y == Nothing = Nothing 
    | otherwise = Just (Cow s x y)

cowFromString2 :: String -> Int -> Int -> Maybe Cow
cowFromString2 s x y = (fmap Cow (noEmpty s)) <*> (noNegative x) <*> (noNegative y)

newtype Name = Name String
    deriving (Eq, Show)
newtype Address = Address String
    deriving (Eq, Show)
data Person = Person Name Address
    deriving (Eq, Show)

validateLength :: Int -> String -> Maybe String 
validateLength n s = if length s <= n then Just s else Nothing

mkName :: String -> Maybe Name
mkName s 
    | validateLength 25 s /= Nothing = Just (Name s)
    | otherwise = Nothing

mkAddress :: String -> Maybe Address
mkAddress s
    | validateLength 100 s /= Nothing = Just (Address s)
    | otherwise = Nothing

mkPerson1 :: String -> String -> Maybe Person
mkPerson1 s1 s2
    | mkName s1 == Nothing || mkAddress s2 == Nothing = Nothing
    | otherwise = Just (Person (Name s1) (Address s2))

mkPerson2 :: String -> String -> Maybe Person
mkPerson2 s1 s2 = fmap Person (mkName s1) <*> mkAddress s2