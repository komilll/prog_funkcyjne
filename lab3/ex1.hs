f1 :: Num a => a -> a
f1 = \x -> x - 2

f2 :: Floating a => (a, a) -> a
f2 = \(x, y) -> sqrt(x^2 + y^2)

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

f3 :: (Int, Int, Int) -> Int
f3 = \(x, y, z) -> isqrt(x^2 + y^2 + z^2)

f7 :: Int -> Bool
f7 = \x -> x `mod` 2 == 0

f8 :: Floating a => a -> a
f8 x = (\y -> (y $ sqrt x))
        (\y -> 2 * y^3 * (y + 1))

f9 :: Int -> Int
f9 = \x -> case x of
            1 -> 3
            _ -> 0