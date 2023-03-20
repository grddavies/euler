-- Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
--
-- If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair
-- and each of a and b are called amicable numbers.
--
-- For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110;
-- therefore d(220) = 284.
-- The proper divisors of 284 are 1, 2, 4, 71 and 142;
-- So d(284) = 220
--
-- Evaluate the sum of all the amicable numbers under 10000.

import Primes (primeFactorsGroup)

divisors n = product <$> mapM (\(p, k) -> [p^x | x <- [0..k]]) (primeFactorsGroup n)

d n = sum (divisors n) - n

amicableNumbers = [ n | n <- [2..],  n == d (d n), n /= d n ]

solve n = sum $ takeWhile (< n) amicableNumbers

main :: IO ()
main = (print . sum) $ takeWhile (< 10000) amicableNumbers

