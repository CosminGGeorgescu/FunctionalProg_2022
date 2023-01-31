{-
class Functor f where
fmap : : ( a -> b ) -> f a -> f b
-}
-- newtype =: compilatorul face optimizari in anumite situatii
newtype Identity a = Identity a

data Pair a = Pair a a
instance Functor Pair where
    fmap f (Pair a b) = Pair  (f a) (f b)

-- a = tip fantoma
data Constant a b = Constant b
instance Functor (Constant a) where
    fmap f(Constant b) = Constant (f b)


data Two a b = Two a b
instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)
    
data Three a b c = Three a b c
instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)
    
data Three' a b = Three' a b b
instance Functor (Three' a) where
    fmap f (Three' a b c) = Three' a (f b) (f c)

data Four a b c d = Four a b c d
instance Functor (Four a b c) where
    fmap f(Four a b c d) = Four a b c (f d)
    
data Four'' a b = Four'' a a a b
instance Functor (Four'' a) where
    fmap f (Four'' a b c d) = Four'' a b c (f d)

data Quant a b = Finance | Desk a | Bloor b
instance Functor (Quant a) where
    fmap f Finance = Finance
    fmap f (Desk a) = Desk a
    fmap f (Bloor b) = Bloor (f b)

data LiftItOut f a = LiftItOut (f a)
instance Functor f => Functor (LiftItOut f) where
    fmap aToB (LiftItOut a) = LiftItOut (fmap aToB a)

data Parappa f g a = DaWrappa (f a) (g a)
instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap f1 (DaWrappa a b) = DaWrappa (fmap f1 a) (fmap f1 b)

data IgnoreOne f g a b = IgnoringSomething (f a) (g b)
instance (Functor f, Functor g) => Functor (IgnoreOne f g a) where
    fmap f1 (IgnoringSomething a b) = IgnoringSomething a (fmap f1 b)

data Notorious g o a t = Notorious (g o) (g a) (g t)
instance Functor g => Functor (Notorious g o a) where
    fmap f1 (Notorious a b c) = Notorious a b (fmap f1 c)
    
data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
instance Functor GoatLord where
    fmap f1 NoGoat = NoGoat
    fmap f1 (OneGoat a)= OneGoat (f1 a)
    fmap f1 (MoreGoats a b c) = MoreGoats (fmap f1 a) (fmap f1 b) (fmap f1 c)

data TalkToMe a = Halt | Print String a | Read (String -> a)
instance Functor TalkToMe where
    fmap f1 Halt = Halt
    fmap f1 (Print a b) = Print a (f1 b)
    fmap f1 (Read a) = Read (fmap f1 a)