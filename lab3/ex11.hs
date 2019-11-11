-- concat' :: [[a]] -> [a]
-- concat' [] = []
-- concat' (x:xs) = x ++ concat' xs

-- concat' :: [[a]] -> [a]
-- concat' xss = [x | xs <- xss, x <- xs]

concat' xs = foldr (++) [] xs