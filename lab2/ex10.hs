fst2Eq :: Eq a => [a] -> Bool
fst2Eq (x : y : _) | x == y = True
fst2Eq _                    = False

fst2Div :: [Int] -> Bool
fst2Div (x : y : _) | x `mod` y == 0 = True
fst2Div _                        = False

mod3Elem :: [Int] -> Bool
mod3Elem (x:y:z:_) | z `mod` x == 0 = True
mod3Elem _                          = False