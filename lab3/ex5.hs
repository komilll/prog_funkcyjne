import Data.List

sortDesc :: Ord a => [a] -> [a]
sortDesc xs = (reverse . sort) xs

are2FunsEqAt :: Eq a => (t -> a) -> (t -> a) -> [t] -> Bool
are2FunsEqAt f g xs = length (filter (\x -> f(x) == g(x)) xs) == length (xs)

-- infixl 9 >.>
-- (>.>) :: (a -> b) -> (b -> c) -> (a -> c)
-- g >.> f = 

composeFunList :: [a -> a] -> (a -> a)
composeFunList [] = id
composeFunList (f:fs) = f . composeFunList fs