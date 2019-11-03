sqr x = x^2

funcFactory n = case n of
    1 -> id
    2 -> sqr
    3 -> (^3)
    4 ->  \x -> x^4
    5 -> intFunc
    _ -> const n
    where
        intFunc x = x^5

expApproxUpTo :: Int -> Double -> Double
expApproxUpTo n x = helper n x 0
                where 
                    helper :: Int -> Double -> Double -> Double
                    helper n x acc
                        | n == 0 = 1 + acc
                        | otherwise = helper (n-1) x (acc + (x^n/ fromIntegral (fac(n))))
                    fac :: Int -> Int
                    fac n
                            | n == 0 = 1
                            | otherwise = n * fac (n - 1)

dfr :: (Double -> Double) -> Double -> (Double -> Double)
dfr f h = \x -> (f(x + h) - f(x)) / h

dfc :: (Double -> Double) -> Double -> (Double -> Double)
dfc f h = \x -> (f(x + h) - f(x - h) / 2*h)