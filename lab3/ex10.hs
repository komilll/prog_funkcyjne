isSortedAsc :: Ord a => [a] -> Bool
isSortedAsc xs = length xs - 1 == (length $ filter (\(x,y) -> x <= y) $ zip xs (tail xs))

everySecond :: [t] -> [Int]
everySecond xs = map (\(x,y) -> y) $ filter(\(x,y) -> y `mod` 2 == 1) $ zip xs [1..length xs]

zip3' :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3' _ _ [] = []
zip3' _ [] _ = []
zip3' [] _ _ = []
zip3' (a:as) (b:bs) (c:cs) = (a,b,c) : zip3' as bs cs

-- unzip3' :: [(a, b, c)] -> ([a], [b], [c])
-- unzip3' [] = []
-- unzip3' (x:xs) = 

isSortedDesc :: Ord a => [a] -> Bool
isSortedDesc xs = length xs - 1 == (length $ filter (\(x,y) -> x >= y) $ zip xs (tail xs))

isSorted :: Ord a => [a] -> Bool
isSorted xs = isSortedAsc xs || isSortedDesc xs

fibs = 0 : 1 : zipWith (+) fibs (tail fibs) :: [Int]