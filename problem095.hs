-- The proper divisors of a number are all the divisors excluding the number itself.
-- For example, the proper divisors of 28 are 1, 2, 4, 7, and 14.
-- As the sum of these divisors is equal to 28, we call it a perfect number.
--
-- Interestingly the sum of the proper divisors of 220 is 284 and the sum of the proper divisors of 284 is 220, forming a chain of two numbers. For this reason, 220 and 284 are called an amicable pair.
--
-- Perhaps less well known are longer chains. For example, starting with 12496, we form a chain of five numbers:
--
-- 12496 → 14288 → 15472 → 14536 → 14264 (→ 12496 → ...)
--
-- Since this chain returns to its starting point, it is called an amicable chain.
--
-- Find the smallest member of the longest amicable chain with no element exceeding one million.

import Data.List (foldl1', group, unfoldr, sortOn, maximumBy)
import qualified Data.Set as Set
import Data.Ord (comparing)
import Control.Monad (guard)

import Primes (primesInt)

-- Upper bound for chain members
upper = 999999

-- This is equivalent to the much friendlier 'do' notation below
divisorsLcomp n = 1 : [y | x <- takeWhile (\y -> y^2 <= n) [2..], let (q, r) = quotRem n x, r == 0, y <- if q /= x then [x, q] else [x]]

-- Find divisors by trial division
divisorsDo n = 1 : do
  x <- takeWhile (\y -> y ^ 2 <= n) [2..]
  let (q, r) = quotRem n x
  guard $ r == 0
  y <- if x /= q then [x, q] else [x]
  return y

-- Trial division by primes
primeFactors n = factors n primesInt
 where
  factors 1 _                  = []
  factors m (p:ps) | m < p*p   = [m]
                   | r == 0    = p : factors q (p:ps)
                   | otherwise = factors m ps
   where (q,r) = quotRem m p

primeFactorsGroup2 n = factors n primesInt
 where
  factors 1 _                  = []
  factors m (p:ps) | m < p*p   = [(m, 1)]
                   | otherwise = (p, a) : factors q ps
   where (a, q, r) = myDiv m p
         -- Should probably use unfoldr not takeWhile and iterate
         myDiv m p = last $ takeWhile (\(a, q, r) -> r == 0) $ iterate (\(a, m', r) -> if r == 0 then let (q, r) = m' `quotRem` p in (a+1, q, r) else (a, m', r)) (0, m, 0)

-- Run length encode primeFactors
primeFactorsGroup n = map (\xs -> (head xs, length xs)) $ group $ primeFactors n

-- List the proper divisors of a number
divisors n = foldl1 (*) <$> (sequence $ map (\(p, k) -> [p^x | x <- [0..k]]) facs)
  where
    facs = primeFactorsGroup n

-- (Sorted) List the proper divisors of a number
divisors2 n = foldr (\(p, e) ds -> concatMap (\d -> [d*p^ee | ee <- [0..e]]) ds) [1] $ primeFactorsGroup n

sumProperDivs d = (sum $ divisors2 d) - d

amicableChainS n = go n n Set.empty
  where
    go first current visited
      | current `Set.member` visited = if first == current then visited else Set.empty
      | current == 1                 = Set.empty
      | current > upper              = Set.empty
      | otherwise                    = go first next (Set.insert current visited)
      where
        next = sumProperDivs current

amicableChainList n = go n n []
  where
    go first current visited
      | current `elem` visited = if first == current then visited else []
      | current == 1           = []
      | current > upper        = []
      | otherwise              = go first next (current : visited)
      where
        next = sumProperDivs current

chainsList = map amicableChainList [2..upper]

chainsSet = map amicableChainS [2..upper]

solveList = foldl1' (min) $ maximumBy (comparing length) $ chainsList

solve = Set.findMin $ maximumBy (comparing Set.size) $ chainsSet


main = putStrLn . show $ solve

