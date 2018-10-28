module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance x y = if length x == length y
    then Just (length (filter (\a -> not a) (zipWith (==) x y)))
    else Nothing
