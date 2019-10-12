fiveToPower_ :: Integer -> Integer
fiveToPower_ = (5 ^)

_ToPower5 :: Num a => a -> a
_ToPower5 = (^ 5)

substrNFrom5 :: Num a => a -> a
substrNFrom5 = (5 - )

substr5From_ :: Num a => a -> a
substr5From_ = (+(-5))

flip2 :: (a -> b -> c) -> b -> a -> c
flip2 f a b = f b a