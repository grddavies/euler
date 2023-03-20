module Primes where

import qualified Data.Map.Strict as M
import qualified Data.List as L

{-# SPECIALISE primes :: [Int] #-}
{-# SPECIALISE primes :: [Integer] #-}
-- Infinite list of primes
primes :: Integral a => [a]
primes = sieve [2 ..]

{-# SPECIALISE primeFactors :: Int -> [Int] #-}
{-# SPECIALISE primeFactors :: Integer -> [Integer] #-}
-- Prime factors by trial division of primes
primeFactors :: Integral a => a -> [a]
primeFactors n = factors n primes
 where
  factors 1 _                  = []
  factors m (p:ps) | m < p*p   = [m]
                   | r == 0    = p : factors q (p:ps)
                   | otherwise = factors m ps
   where (q,r) = quotRem m p

{-# SPECIALISE primeFactorsGroup :: Int -> [(Int, Int)] #-}
{-# SPECIALISE primeFactorsGroup :: Integer -> [(Integer, Int)] #-}
-- List of tuples of prime factors and their exponents
primeFactorsGroup :: Integral a => a -> [(a, Int)]
primeFactorsGroup n = map (\xs -> (head xs, length xs)) $ L.group $ primeFactors n

{-# SPECIALISE eulerTotient :: Int -> Int #-}
{-# SPECIALISE eulerTotient :: Integer -> Integer #-}
-- Euler totient function
eulerTotient :: Integral a => a -> a
eulerTotient n = totient n
  where
    totient 1 = 1
    totient n = product [p^(k-1) * (p-1) | (p, k) <- primeFactorsGroup n]

-- https://doi.org/10.1017/S0956796808007004
sieve :: Integral a => [a] -> [a]
sieve xs = sieve' xs M.empty
  where
    sieve' [] table = []
    sieve' (x : xs) table = case M.lookup x table of
      Nothing    -> x : sieve' xs (M.insert (x * x) [x] table)
      Just facts -> sieve' xs (foldl reinsert (M.delete x table) facts)
      where
        reinsert table prime = M.insertWith (++) (x + prime) [prime] table

