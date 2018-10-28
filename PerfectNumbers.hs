module PerfectNumbers (classify, Classification(..)) where
    
import Control.Monad (guard)

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify x
    | x < 1 = Nothing
    | otherwise = case compare (sum (divisors x)) x of
        EQ -> Just Perfect
        LT -> Just Deficient
        GT -> Just Abundant

divisors :: Int -> [Int]
divisors x = do
    a <- [1..x - 1]
    guard $ x `mod` a == 0
    pure $ a