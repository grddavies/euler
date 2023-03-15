-- The number, 197, is called a circular prime because all rotations of the
-- digits: 197, 971, and 719, are themselves prime.
--
-- There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37,
-- 71, 73, 79, and 97.
--
-- How many circular primes are there below one million?

import Primes (primes)
import Data.List (permutations, all)

main :: IO ()
main = putStrLn . show $ solve 1000000

solve :: Integer -> Int
solve n = length $ filter isCircularPrime $ takeWhile (< n) primes

isCircularPrime :: Integer -> Bool
isCircularPrime x = all (flip member primes) (map read . tail . rotations $ show x)

rotate :: Int -> [a] -> [a]
rotate = drop <> take

rotations :: [a] -> [[a]]
rotations xs = [rotate n xs | n <- [0..(length xs - 1)]]

member :: Ord a => a -> [a] -> Bool
member x xs = x `elem` takeWhile (<= x) xs

