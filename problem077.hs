-- It is possible to write ten as the sum of primes in exactly five different
-- ways:
--
-- 7 + 3
-- 5 + 5
-- 5 + 3 + 2
-- 3 + 3 + 2 + 2
-- 2 + 2 + 2 + 2 + 2
--
-- What is the first value which can be written as the sum of primes in over
-- five thousand different ways?

import Data.List (findIndex)
import Data.Maybe (maybe)

import Primes (primes)

ppc = p primes
  where
   p _          0 = 1
   p ks'@(k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

solve n = findIndex (>n) $ map ppc [0..]

main = putStrLn . maybe "Nothing" show $ solve 5000

