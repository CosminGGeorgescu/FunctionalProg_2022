data Expr = Const Int -- integer constant
          | Expr :+: Expr -- addition
          | Expr :*: Expr -- multiplication
    deriving Eq
data Operation = Add | Mult 
    deriving (Eq, Show)
data Tree = Lf Int -- leaf
          | Node Operation Tree Tree -- branch
    deriving (Eq, Show)

instance Show Expr where
    show (Const x) = show x
    show (e1 :+: e2) = show e1 ++ " + " ++ show e2
    show (e1 :*: e2) = show e1 ++ " * " ++ show e2

evalExp :: Expr -> Int
evalExp (Const x) = x
evalExp (e1 :+: e2) = evalExp e1 + evalExp e2
evalExp (e1 :*: e2) = evalExp e1 * evalExp e2

evalArb :: Tree -> Int
evalArb (Lf x) = x
evalArb (Node Mult l1 l2) = evalArb l1 * evalArb l2
evalArb (Node Add l1 l2) = evalArb l1 + evalArb l2

expToArb :: Expr -> Tree
expToArb (Const x) = Lf x
expToArb (e1 :+: e2) = Node Add (expToArb e1) (expToArb e2)
expToArb (e1 :*: e2) = Node Mult (expToArb e1) (expToArb e2)

class Collection c where
    empty :: c key value
    singleton :: key -> value -> c key value
    insert :: Ord key => key -> value -> c key value -> c key value
    clookup :: Ord key => key -> c key value -> Maybe value
    delete :: Ord key => key -> c key value -> c key value
    keys :: c key value -> [key]
    keys c = [fst p | p <- toList c]
    values :: c key value -> [value]
    values c = [snd p | p <- toList c]
    toList :: c key value -> [(key, value)]
    fromList :: Ord key => [(key,value)] -> c key value
    fromList [] = empty
    fromList (x : xs) = insert (fst x) (snd x) (fromList xs)
newtype PairList k v = PairList { getPairList :: [(k, v)] }

instance Collection PairList where
    empty = PairList []
    singleton k v = PairList [(k, v)]
    insert k v (PairList list) = PairList ((k, v) : filter (\x -> fst x /= k) list)
    clookup k (PairList list) = lookup k list
    delete k (PairList list) = PairList (filter (\x -> fst x /= k) list)
    toList (PairList list) = list

-- atat s-a putut