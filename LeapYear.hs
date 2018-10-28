module LeapYear (isLeapYear) where

isLeapYear :: Integer -> Bool
isLeapYear year = isDivBy year 4 && (not (isDivBy year 100) || isDivBy year 400)

isDivBy x y = x `mod` y == 0
