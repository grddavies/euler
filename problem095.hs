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
import qualified Data.IntSet as Set
import Data.Ord (comparing)
import Control.Monad (guard)

import Primes (primesInt)

-- Upper bound for chain members
upper = 999999

-- Trial division by primes
primeFactors n = factors n primesInt
 where
  factors 1 _                  = []
  factors m (p:ps) | m < p*p   = [m]
                   | r == 0    = p : factors q (p:ps)
                   | otherwise = factors m ps
   where (q,r) = quotRem m p

-- Run length encode primeFactors
primeFactorsGroup n = map (\xs -> (head xs, length xs)) $ group $ primeFactors n

-- (Sorted) List the proper divisors of a number
divisors n = foldr (\(p, e) ds -> concatMap (\d -> [d*p^ee | ee <- [0..e]]) ds) [1] $ primeFactorsGroup n

sumProperDivs d = (sum $ divisors d) - d

amicableChain n = go n n Set.empty
  where
    go first current visited
      | current `Set.member` visited = if first == current then visited else Set.empty
      | current == 1                 = Set.empty
      | current > upper              = Set.empty
      | otherwise                    = go first next (Set.insert current visited)
      where
        next = sumProperDivs current

chains = map amicableChain [2..upper]

solve = Set.findMin $ maximumBy (comparing Set.size) $ chains

main = putStrLn . show $ solve

