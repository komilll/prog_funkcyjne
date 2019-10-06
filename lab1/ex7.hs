not' :: Bool -> Bool
not' True = False
not' False = True

isItTheAnswer :: String -> Bool
isItTheAnswer "Love" = True
isItTheAnswer _ = False

or' :: (Bool, Bool) -> Bool
or' (x, y) = (x == y && x == False)

and' :: (Bool, Bool) -> Bool
and' (x, y) = if (x == y && x == True)
                then False 
                else True