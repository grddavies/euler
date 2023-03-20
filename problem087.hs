-- The smallest number expressible as the sum of a prime square, prime cube,
-- and prime fourth power is 28. In fact, there are exactly four numbers
-- below fifty that can be expressed in such a way:
--
-- 28 = 2^2 + 2^3 + 2^4
-- 33 = 3^2 + 2^3 + 2^4
-- 49 = 5^2 + 2^3 + 2^4
-- 47 = 2^2 + 3^3 + 2^4
--
-- How many numbers below fifty million can be expressed as the sum of a
-- prime square, prime cube, and prime fourth power?

import Primes (primes)
import Data.Set (Set)
import qualified Data.Set as S

xs n = S.fromList $ do
  a <- takeWhile (< n)         $ map (^2) primes
  b <- takeWhile (< n - a)     $ map (^3) primes
  c <- takeWhile (< n - a - b) $ map (^4) primes
  return (a + b + c)

solve = S.size . xs

main = print $ solve 50000000

