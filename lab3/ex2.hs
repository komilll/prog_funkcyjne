sumWith :: Num a => (a -> a) -> [a] -> a
sumWith _ [] = 0
sumWith f (x:xs) = f x + sumWith f xs

sum' :: Num a => [a] -> a
sum' = sumWith(\e -> e)

sumSqr' :: Num a => [a] -> a
sumSqr' = sumWith(\e -> e^2)

listLength :: Num a => [a] -> a
listLength = sumWith(\e -> 1)
--------------

prodWith :: Num a => (a -> a) -> [a] -> a
prodWith _ [] = 0
prodWith _ [x] = x
prodWith f (x:xs) = f x * prodWith f xs

prod' :: Num a => [a] -> a
prod' = prodWith(\e -> e)

--------------
{--
funcWith :: Num a => (a -> a) -> [a] -> ((a, a) -> a) -> a
funcWith _ [] _ = 0
funcWith _ [x] _ = x
funcWith f (x:xs) g = f x 
--}