newtype MyInt = MkMyInt Int

instance Eq MyInt where
    (==) (MkMyInt i1) (MkMyInt i2) = i1 == i2

instance Ord MyInt where
    (<=) (MkMyInt i1) (MkMyInt i2) = i1 <= i2

instance Num MyInt where
    (+) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 + i2)
    (-) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 - i2)
    (*) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 * i2)
    negate (MkMyInt i)            = MkMyInt (negate i)
    abs (MkMyInt i)               = MkMyInt (abs i)
    signum (MkMyInt i)            = MkMyInt (signum i)
    fromInteger int               = MkMyInt (fromIntegral int)

instance Show MyInt where
    show (MkMyInt i) = "MkMyInt " ++ show i

--Exercices
--2
data BinTree a = EmptyBT | 
                NodeBT a (BinTree a) (BinTree a)
                -- deriving (Eq, Ord, Show, Read)

instance Eq a => Eq (BinTree a) where
    (==) (EmptyBT) (EmptyBT) = True
    (==) EmptyBT (NodeBT n2 lt2 rt2) = False
    (==) (NodeBT n2 lt2 rt2) EmptyBT = False
    (==) (NodeBT n1 lt1 rt1) (NodeBT n2 lt2 rt2) = n1 == n2 && lt1 == lt2 && rt1 == rt2

data MyDouble = MkMyDouble Double

instance Show MyDouble where
    show (MkMyDouble d) = "MkMyDouble " ++ show d

instance Num MyDouble where
    (+) (MkMyDouble d1) (MkMyDouble d2) = MkMyDouble (d1 + d2)
    (-) (MkMyDouble d1) (MkMyDouble d2) = MkMyDouble (d1 - d2)
    (*) (MkMyDouble d1) (MkMyDouble d2) = MkMyDouble (d1 * d2)
    abs (MkMyDouble d) = MkMyDouble (abs d)
    signum (MkMyDouble d) = MkMyDouble (signum d)
    fromInteger d = MkMyDouble (fromIntegral d)