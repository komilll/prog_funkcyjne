import Data.Char

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
        | p(x)      = x : filter' p xs
        | otherwise = filter' p xs

-- onlyEven [] = []
-- onlyEven (x:xs)
--         | x `mod` 2 == 0 = x : onlyEven xs
--         | otherwise      = onlyEven xs

-- onlyOdd [] = []
-- onlyOdd (x:xs)
--         | x `mod` 2 == 1 = x : onlyOdd xs
--         | otherwise      = onlyOdd xs

-- onlyUpper [] = []
-- onlyUpper (x:xs)
--         | isUpper(x) = x : onlyUpper xs
--         | otherwise      = onlyUpper xs

onlyEven = filter' (\e -> e `mod` 2 == 0)
onlyOdd = filter' (\e -> e `mod` 2 == 1)
onlyUpper = filter' isUpper

filterEvenLength = [ x | x <- [1..10^6], even x]