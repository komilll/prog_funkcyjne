{-# LANGUAGE DeriveFunctor #-}

newtype Box a = MkBox a deriving Show

instance Functor Box where
    fmap f (MkBox x) = MkBox (f x)

instance Applicative Box where
    pure = MkBox
    (MkBox f) <*> w = fmap f w

--Exercises
--1
newtype Pair b a = Pair { getPair :: (a,b) }

instance Functor (Pair c) where
    fmap f (Pair (x,y)) = Pair (f x, y)

instance Applicative Pair where
    pure = Pair
    (Pair f) <*> x <*> y = fmap f x y

-- newtype MyTriple a = MyTriple { getMyTriple :: (a,a,a) } 

-- instance Functor (MyTriple c) where
--     fmap f (MyTriple (x,y,z)) = MyTriple (f x, y, z)

-- instance Applicative MyTriple where
--     pure = MkMyTriple
--     (MkMyTriple f) <*> x = fmap f x