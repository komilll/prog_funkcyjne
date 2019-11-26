data BinIntTree = EmptyIntBT |
                  IntNodeBT Int BinIntTree BinIntTree

sumBinIntTree :: BinIntTree -> Int
sumBinIntTree EmptyIntBT = 0
sumBinIntTree (IntNodeBT n lt rt) = n + sumBinIntTree lt + sumBinIntTree rt

data BinTree a = EmptyBT | 
                 NodeBT a (BinTree a) (BinTree a)
                 deriving (Eq, Ord, Show, Read)

sumBinTree :: (Num a) => BinTree a -> a
sumBinTree EmptyBT = 0
sumBinTree (NodeBT n lt rt) = n + sumBinTree lt + sumBinTree rt

data Expr a = Lit a |
              Add (Expr a) (Expr a) |
              Substract (Expr a) (Expr a)

eval :: Num a => Expr a -> a
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Substract e1 e2) = eval e1 - eval e2

show' :: Show a => Expr a -> String
show' (Lit n) = show n
show' (Add e1 e2) = "(" ++ show' e1 ++ "+" ++ show' e2 ++ ")"
show' (Substract e1 e2) = "(" ++ show' e1 ++ "-" ++ show' e2 ++ ")"

-- Exercices --
--1
depthOfBT :: BinTree a -> Int
depthOfBT EmptyBT = 0
depthOfBT (NodeBT n lt rt) = 1 + max (depthOfBT lt) (depthOfBT rt)

flattenBT :: BinTree a -> [a]
flattenBT EmptyBT = []
flattenBT (NodeBT n lt rt) = flattenBT lt ++ [n] ++ flattenBT rt

mapBT :: (a -> b) -> BinTree a -> BinTree b
mapBT f EmptyBT = EmptyBT
mapBT f (NodeBT n lt rt) = NodeBT (f n) (mapBT f lt) (mapBT f rt)

insert :: Ord a => a -> BinTree a -> BinTree a
insert x EmptyBT = NodeBT x EmptyBT EmptyBT
insert x (NodeBT n lt rt)
            | n == x = NodeBT n lt rt
            | n < x = NodeBT n lt (insert x rt)
            | n > x = NodeBT n (insert x lt) rt

list2BST :: Ord a => [a] -> BinTree a
list2BST [] = EmptyBT
list2BST (x:xs) = list2BSTCon (NodeBT x EmptyBT EmptyBT) xs
    where
        list2BSTCon t [] = t
        list2BSTCon t (x:xs) = list2BSTCon (insert x t) xs

--2
occurs :: Eq a => a -> BinTree a -> Int
occurs _ EmptyBT = 0
occurs val (NodeBT n lt rt) = occursCon val (NodeBT n lt rt) 0
        where 
            occursCon val EmptyBT acc = acc
            occursCon val (NodeBT n lt rt) acc
                                            | val == n = acc + 1 + occursCon val lt 0 + occursCon val rt 0
                                            | otherwise = acc + occursCon val lt 0 + occursCon val rt 0

elemOf :: Eq a => a -> BinTree a-> Bool
elemOf _ EmptyBT = False
elemOf val t = elemOfCon val t
            where 
                elemOfCon val EmptyBT = False
                elemOfCon val (NodeBT n lt rt)
                                            | n == val = True
                                            | otherwise = (elemOfCon val lt) || (elemOfCon val rt)

-- minElemOf :: Ord a => a -> BinTree a -> a
-- minElemOf val EmptyBT = val
-- minElemOf val (NodeBT n lt rt) =
--     let leftMin = minElemOf val lt
--         rightMin = minElemOf val rt
--     in if (leftMin < rightMin)
--         then
--             leftMin
--         else 
--             rightMin

--3