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

concat' :: [[a]] -> [a]
concat' [[]] = []
concat' [] = []
concat' (xs:ys) = xs ++ concat'(ys)

concat'' :: [[a]] -> [a]
concat'' xss = [x | xs <- xss, x <- xs]

isSorted :: [Int] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:y:xs) = if x <= y
                    then isSorted(y:xs)
                    else False

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse'(xs) ++ [x]

zip' :: [a] -> [b] -> [(a,b)]
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys
zip' _  _   = []