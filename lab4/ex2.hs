data CartInt2DVec = MkCartInt2DVec Int Int

xCoord :: CartInt2DVec -> Int
xCoord (MkCartInt2DVec x _) = x

yCoord :: CartInt2DVec -> Int
yCoord (MkCartInt2DVec _ y) = y
--------------
data Cart2DVec' a = MkCart2DVec' a a 

xCoord' :: Cart2DVec' a -> a
xCoord' (MkCart2DVec' x _) = x

yCoord' :: Cart2DVec' a -> a
yCoord' (MkCart2DVec' _ y) = y
--------------
data Cart2DVec'' a = MkCart2DVec'' {x::a, y::a}

-- xCoord'' :: Cart2DVec'' a -> a
-- xCoord'' (MkCart2DVec'' {x = xVal, y = _}) = xVal

-- yCoord'' :: Cart2DVec'' a -> a
-- yCoord'' (MkCart2DVec'' {y = yVal, x = _}) = yVal

data List a = EmptyL | Cons a (List a) deriving Show

head' :: List a -> a
head' EmptyL = error "head' : the empty list has no head!"
head' (Cons x xs) = x

-- data ThreeColors = Blue |
--                    White |
--                    Red

-- type ActorName = String

-- leadingActor :: ThreeColors -> ActorName
-- leadingActor Blue = "Juliette Binoche"
-- leadingActor White = "Zbigniew Zamachowski"
-- leadingActor Red = "Irene Jacob"

-- Exercices --
--1
-- data Cart3DVec a = MkCart3DVec a a a
-- xCoord3D :: Cart3DVec a -> a
-- xCoord3D (MkCart3DVec x _ _) = x

--2
data Cart3DVec a = MkCart3DVec{x3D::a, y3D::a, z3D::a}

--6
data Circle a = MkCircle a
data Rectangle a = MkRectangle a a
data Shape = Circle Float | Rectangle Float Float
area :: Shape -> Float
area (Circle r) = 3.14 * r * r
area (Rectangle a b) = a * b

--7
data Tree a = EmptyT | 
                Node a (Tree a) (Tree a) 
                deriving Show

rootValue :: Tree a -> a
rootValue EmptyT = error ("Empty tree")

--8
data TrafficLights = Red |
                     Yellow |
                     Green

actionFor :: TrafficLights -> DriveAction
actionFor Red = Stop
actionFor Yellow = Prepare
actionFor Green = Go

--9
data DriveAction = Stop |
                   Prepare |
                   Go
                    deriving Show