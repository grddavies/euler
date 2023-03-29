import Data.Array (listArray, (!))

import Primes (primeFactorsGroup)

divisors = foldr (\(p, e) ds -> concatMap (\d -> [d*p^x | x <- [0..e]]) ds) [1] . primeFactorsGroup

-- sigma0 n = Number of divisors of n
s:: Int -> Int
s = product . map ((+1) . snd) . primeFactorsGroup

-- Naive implementation of the function D
f m n = sum $ [s (k*d) | d <- ds, k <- [1..n]]
  where
    ds = divisors m

-- s is multiplicative, so we can roll into the factorial function
sFact n = go n 1
  where
    go 1 acc = acc
    go n acc = go (n-1) (acc * s n)


