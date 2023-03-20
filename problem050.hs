-- The prime 41, can be written as the sum of six consecutive primes:
--
-- 41 = 2 + 3 + 5 + 7 + 11 + 13
-- This is the longest sum of consecutive primes that adds to a prime below one-hundred.
--
-- The longest sum of consecutive primes below one-thousand that adds to a prime, contains 21 terms, and is equal to 953.
--
-- Which prime, below one-million, can be written as the sum of the most consecutive primes?

import qualified Data.Set as Set
import Data.List (tails, minimumBy)
import Data.Ord (comparing)
import Data.Function (on)

import Primes (primes)


solve n = head
          $ minimumBy (flip compare `on` length)
          $ fmap (dropWhile (not . isPrime) . reverse)
          $ takeWhile (not . null)
          $ takeWhile (< n)
         <$> map (scanl1 (+)) (tails primes)
  where
    primesSet = Set.fromList $ takeWhile (< n) primes
    isPrime :: Int -> Bool
    isPrime = (`Set.member` primesSet)

main :: IO ()
main = print $ solve 1000000

