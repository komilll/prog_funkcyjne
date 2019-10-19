isqrt :: Integer -> Integer
isqrt x = floor(sqrt(fromIntegral(x)))

--isPrime :: Integer t => t -> Bool
--isPrime n = [i | i <- [2..isqrt n], n `mod` i == 0] == []

isPrime :: Integral t => t -> Bool
isPrime n = [i | i <- [2..n], n `mod` i == 0] == []

primes :: [Int]
primes = eratoSive[2..]
    where
        eratoSive :: [Int] -> [Int]
        eratoSive (p : xs) = p : eratoSive [x | x <- xs, x `mod` p /= 0]

primesInRange :: Int -> [Int]
primesInRange n = eratoSive[2..n]
    where
        eratoSive :: [Int] -> [Int]
        eratoSive [] = []
        eratoSive (p : xs) = 
                            if p > n
                                then xs
                                else p : eratoSive [x | x <- xs, x `mod` p /= 0]

allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual xs = if (length xs == 1) 
                then True
                else
                    if (xs !! 0 == xs !! 1)
                        then allEqual(tail xs)
                        else False