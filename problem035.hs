-- The number, 197, is called a circular prime because all rotations of the
-- digits: 197, 971, and 719, are themselves prime.
--
-- There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37,
-- 71, 73, 79, and 97.
--
-- How many circular primes are there below one million?

import Primes (primes)
import qualified Data.Set as Set
import Data.List (permutations, all)

main :: IO ()
main = putStrLn . show $ solveStr 1000000

-- String rotation seems faster in ghci

solveStr :: Integer -> Int
solveStr n = Set.size $ Set.filter isCircularPrime $ primesBelowN
  where
    primesBelowN = Set.fromList $ takeWhile (< n) primes
    isCircularPrime x = all (flip Set.member primesBelowN) (map read . tail . rotationsL $ show x)

solveInt n = Set.size $ Set.filter isCircularPrime $ primesBelowN
  where
    primesBelowN = Set.fromList $ takeWhile (< n) primes
    isCircularPrime x = all (flip Set.member primesBelowN) (tail $ rotations x)

-- Count the digits in a positive nonzero number
digitCount :: Integer -> Integer
digitCount = go 0 
  where
    go ds 0 = ds
    go ds n = go (ds + 1) (n `div` 10)

-- Rotate the first k digits of a positive integer
rotateInt :: Integer -> Integer -> Integer
rotateInt k x = first + rest
  where
    first = x `div` n
    rest = (x `mod` n) * 10 ^ k
    n = 10 ^ ((digitCount x) - k)

-- All rotations of an integer
rotations :: Integer -> [Integer]
rotations x = [rotateInt n x | n <- [0..((digitCount x) - 1)]]

rotate :: Int -> [a] -> [a]
rotate = drop <> take

-- All rotations of a list
rotationsL :: [a] -> [[a]]
rotationsL xs = [rotate n xs | n <- [0..(length xs - 1)]]

