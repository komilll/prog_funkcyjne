import Data.Char

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

-- doubleElems [] = []
-- doubleElems (x:xs) = 2 * x : doubleElems xs

-- sqrElems [] = []
-- sqrElems (x:xs) = x^2 : sqrElems xs

-- lowerCase [] = []
-- lowerCase (x:xs) = toLower x : lowerCase xs

-- doubleElems = map' (\e -> e * 2)
-- sqrElems = map' (\e -> e ^ 2)
-- lowerCase = map' toLower

doubleElems xs = [ x * 2 | x <- xs]
sqrElems xs = [ x ^ 2 | x <- xs]
lowerCase xs = [ toLower x | x <- xs]

evalFuncListAt :: a -> [a -> b] -> [b]
evalFuncListAt x = map ($ x)