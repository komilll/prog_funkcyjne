fst2Eq :: Eq a => [a] -> Bool
fst2Eq (x : y : _) | x == y = True
fst2Eq _                    = False

fst2Div :: [Int] -> Bool
fst2Div (x : y : _) | x `mod` y == 0 = True
fst2Div _                        = False