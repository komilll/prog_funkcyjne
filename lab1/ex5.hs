sgn :: Int -> Int
sgn n = if n < 0
        then -1
        else if n == 0
            then 0
            else 1

absInt :: Int -> Int
absInt n = if (sgn(n) == -1)
            then n * (-1)
            else n

min2Int :: (Int, Int) -> Int
min2Int (x, y) = if (x > y)
                then y
                else x

min3Int :: (Int, Int, Int) -> Int
min3Int (x, y, z) = min2Int(min2Int(x, y), z)

toUpper :: Char -> Char
toUpper (c) = if c >= 'a' && c <= 'z'
                then toEnum(fromEnum c - 32)
                else c

toLower :: Char -> Char
toLower (c) = if c >= 'A' && c <= 'Z'
                then toEnum(fromEnum c + 32)
                else c

isDigit :: Char -> Bool
isDigit (c) = if (c >= '0' && c <= '9')
                then True
                else False

charToNum :: Char -> Int
charToNum (c) = if (isDigit(c))
                then fromEnum(c) - 48 
                else -1

romanDigit :: Char -> String
romanDigit (c) = 
                    if (c == '0') then ""
                    else if (c == '1') then "I"
                    else if (c == '1') then "I"
                    else if (c == '2') then "II"
                    else if (c == '3') then "III"
                    else if (c == '4') then "IV"
                    else if (c == '5') then "V"
                    else if (c == '6') then "VI"
                    else if (c == '7') then "VII"
                    else if (c == '8') then "VIII"
                    else if (c == '9') then "IX"
                    else ""