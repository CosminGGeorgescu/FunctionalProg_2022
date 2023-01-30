{-
class Functor f where
fmap : : ( a -> b ) -> f a -> f b
-}
-- newtype =: compilatorul face optimizari in anumite situatii
newtype Identity a = Identity a

data Pair a = Pair a a
instance Functor Pair where
    fmap::(a->b)->Pair a->Pair b
    fmap f (Pair a b) = Pair  (f a) (f b)

-- a = tip fantoma
data Constant a b = Constant b
instance Functor (Constant a) where
    fmap :: (a2->b) -> Constant a1 a2 -> Constant a1 b
    fmap f(Constant b) = Constant (f b)


data Two a b = Two a b
instance Functor (Two a) where
    fmap :: (a2 -> b) -> Two a1 a2 -> Two a1 b
    fmap f (Two a b) = Two a (f b)
    
data Three a b c = Three a b c
instance Functor (Three a b) where
    fmap :: (a2 -> b2) -> Three a1 b1 a2 -> Three a1 b1 b2
    fmap f (Three a b c) = Three a b (f c)
    
data Three' a b = Three' a b b
instance Functor (Three' a) where
    fmap :: (a2 -> b) -> Three' a1 a2 -> Three' a1 b
    fmap f (Three' a b c) = Three' a (f b) (f c)

data Four a b c d = Four a b c d
instance Functor (Four a b c) where
    fmap :: (a2 -> b2) -> Four a1 b1 c a2 -> Four a1 b1 c b2
    fmap f(Four a b c d) = Four a b c (f d)
    
data Four'' a b = Four'' a a a b
instance Functor (Four'' a) where
    fmap :: (a2 -> b) -> Four'' a1 a2 -> Four'' a1 b
    fmap f (Four'' a b c d) = Four'' a b c (f d)

data Quant a b = Finance | Desk a | Bloor b
instance Functor (Quant a) where
    fmap :: (a2 -> b) -> Quant a1 a2 -> Quant a1 b
    fmap f Finance = Finance
    fmap f (Desk a) = Desk a
    fmap f (Bloor b) = Bloor (f b)

data LiftItOut f a = LiftItOut (f a)
instance Functor f => Functor (LiftItOut f) where
    fmap :: Functor f => (a -> b) -> LiftItOut f a -> LiftItOut f b
    fmap aToB (LiftItOut a) = LiftItOut (fmap aToB a)

data Parappa f g a = DaWrappa (f a) (g a)
instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap :: (Functor f, Functor g) => (a -> b) -> Parappa f g a -> Parappa f g b
    fmap f1 (DaWrappa a b) = DaWrappa (fmap f1 a) (fmap f1 b)

data IgnoreOne f g a b = IgnoringSomething (f a) (g b)
instance (Functor f, Functor g) => Functor (IgnoreOne f g a) where
    fmap :: (Functor f, Functor g) => (a1 -> b) -> IgnoreOne f g a a1 -> IgnoreOne f g a b
    fmap f1 (IgnoringSomething a b) = IgnoringSomething a (fmap f1 b)

data Notorious g o a t = Notorious (g o) (g a) (g t)
instance Functor g => Functor (Notorious g o a) where
    fmap :: Functor g => (a1 -> b) -> Notorious g o a a1 -> Notorious g o a b
    fmap f1 (Notorious a b c) = Notorious a b (fmap f1 c)
    
data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
instance Functor GoatLord where
    fmap :: (a -> b) -> GoatLord a -> GoatLord b
    fmap f1 NoGoat = NoGoat
    fmap f1 (OneGoat a)= OneGoat (f1 a)
    fmap f1 (MoreGoats a b c) = MoreGoats (fmap f1 a) (fmap f1 b) (fmap f1 c)

data TalkToMe a = Halt | Print String a | Read (String -> a)
instance Functor TalkToMe where
    fmap :: (a -> b) -> TalkToMe a -> TalkToMe b
    fmap f1 Halt = Halt
    fmap f1 (Print a b) = Print a (f1 b)
    fmap f1 (Read a) = Read (fmap f1 a)
                    --Read(f.a)

-- fmap ::(a1->b1) -> f a1 -> f b1
-- a1 :: String -> a; b1 :: String -> b