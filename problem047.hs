-- The first two consecutive numbers to have two distinct prime factors are:

-- 14 = 2 × 7
-- 15 = 3 × 5

-- The first three consecutive numbers to have three distinct prime factors are:

-- 644 = 2² × 7 × 23
-- 645 = 3 × 5 × 43
-- 646 = 2 × 17 × 19.

-- Find the first four consecutive integers to have four distinct prime factors each. What is the first of these numbers?
import Data.Set as S ( empty, insert, size )

hasNPrimeFactors :: Int -> Int -> Bool
hasNPrimeFactors n x = factorize' n 2 x S.empty where
  factorize' n x y factors
    | x * x > y = S.size (S.insert y factors) == n
    | y `mod` x == 0 = (S.size (S.insert x factors) < n) && factorize' n x (y `div` x) (S.insert x factors)
    | otherwise = (S.size factors < n) && factorize' n (x + 1) y factors

findFirstN :: Int -> (a -> Bool) -> [a] -> [a]
findFirstN n pred xs
  | prefix xs >= n = take n (takeWhile pred xs)
  | prefix xs > 0 = findFirstN n pred (dropWhile pred xs)
  | otherwise = findFirstN n pred (dropWhile (not . pred) xs)
  where
      prefix = length . takeWhile pred

main :: IO ()
main = print $ head (findFirstN l (hasNPrimeFactors n) [1..])
  where
    l = 4
    n = 4
