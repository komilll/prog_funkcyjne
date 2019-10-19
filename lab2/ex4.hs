import Data.Char

removeFirstLast :: [a] -> [a]
removeFirstLast [] = []
removeFirstLast [x] = []
removeFirstLast xs = tail (init xs)

isPalindrome :: [Char] -> Bool
isPalindrome arr = if (null arr)
                    then True
                    else
                        if (head arr == last arr)
                        then 
                            isPalindrome(removeFirstLast(arr))
                        else False

getElemAtIdx :: ([a], Integer) -> a
getElemAtIdx (xs, index) = 
                            if (index == 0)
                                then head xs
                                else getElemAtIdx(tail xs, index - 1)

capitalize :: [Char] -> [Char]
capitalize arr = map(toUpper) arr