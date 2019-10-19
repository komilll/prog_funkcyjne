cumulativeAmount :: [Integer] -> Integer
cumulativeAmount [] = 0
cumulativeAmount xs = go 0 xs
    where go acc [] = acc
          go acc (x:xs) = go (x+acc) xs

prod'2 :: [Integer] -> Integer
prod'2 [] = 0
prod'2 xs = go 1 xs
    where go acc [] = acc
          go acc (x:xs) = go (x*acc) xs

length'2 :: [a] -> Int
length'2 [] = 0
length'2 xs = go 0 xs
    where go acc [] = acc
          go acc (x:xs) = go(acc + 1) xs