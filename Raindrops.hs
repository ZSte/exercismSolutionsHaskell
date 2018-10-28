module Raindrops (convert) where

convert :: Int -> String
convert n = if divisors n == []
    then show n
    else raindrop (divisors n)

raindrop :: [Int] -> String
raindrop x = mconcat (fmap f x)

f :: Int -> String
f 3 = "Pling"
f 5 = "Plang"
f 7 = "Plong"
f x = ""
    
divisors :: Int -> [Int]
divisors x = [a | a <- [1..x], x `mod` a == 0, a == 3 || a == 5 || a == 7]
