not' :: Bool -> Bool
not' b = case b of
        True -> False
        False -> True

absInt :: Int -> Int
absInt n = 
    case (n >= 0) of
        True -> n
        _ -> -n