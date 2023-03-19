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

import Data.List (maximumBy)
import Data.Ord (comparing)
import qualified Data.IntSet as Set

import Primes (primeFactorsGroup)

-- (Sorted) List the proper divisors of a number
divisors n = foldr (\(p, e) ds -> concatMap (\d -> [d*p^x | x <- [0..e]]) ds) [1] $ primeFactorsGroup n

-- Sum the proper divisors of x
sumpd x = sum (divisors x) - x

amicableChainBelow n lim = go n n Set.empty
  where
    go first current visited
      | current `Set.member` visited = if first == current then visited else Set.empty
      | current == 1                 = Set.empty
      | current > lim                = Set.empty
      | otherwise                    = go first next (Set.insert current visited)
      where
        next = sumpd current

solve n = Set.findMin . maximumBy (comparing Set.size)
        $ map (`amicableChainBelow` n) [2..n]

main = print $ solve 999999

