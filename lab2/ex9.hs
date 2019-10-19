qSort :: Ord a => [a] -> [a]
qSort [] = []
qSort (x:xs) = qSort (leftPart xs) ++ [x] ++ qSort (rightPart xs)
    where
        leftPart xs = filter (<=x) xs
        rightPart xs = filter (>x) xs

-- mSort :: Ord a => [a] -> [a]
-- mSort [] = []
-- mSort [x] = [x]
-- mSort xs = mSort firstHalfSorted secondHalfSorted
-- where
--     firstHalfSorted = mSort

insert :: Int -> [Int] -> [Int]
insert x [] = [x]
insert x (y:ys) = if x < y
                    then x:y:ys
                    else y : insert x ys

iSort :: [Int] -> [Int]
iSort [] = []
iSort (x:xs) = insert x (iSort xs)
