import Data.List

fib :: (Num a, Eq a) => a -> a
fib n = 
    if n == 0 || n == 1 then n
    else fib (n - 2) + fib (n - 1)

sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and'(xs)

doubleAll :: [Int] -> [Int]
doubleAll [] = []
doubleAll xs = doubleAllHelper(0, xs)
    where
        doubleAllHelper :: (Int, [Int]) -> [Int]
        doubleAllHelper (_, []) = []
        doubleAllHelper (a, xs) = if (a < length xs)
                                then doubleAllHelper(a+1, take a xs ++ [(xs !! a) * 2] ++ drop (a+1) xs)
                                else xs

avgAr :: (Real a, Fractional b) => [a] -> b
avgAr xs = realToFrac(sum xs) / genericLength xs