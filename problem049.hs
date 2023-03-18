-- The arithmetic sequence, 1487, 4817, 8147, in which each of the terms increases by 3330, is unusual in two ways:
--   * Each of the three terms are prime
--   * Each of the 4-digit numbers are permutations of one another
--
-- There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes, exhibiting this property, but there is one other 4-digit increasing sequence.
--
-- What 12-digit number do you form by concatenating the three terms in this sequence?

import qualified Data.Set as Set
import Data.List (permutations, group,  sort, find, intercalate)

import Primes (primes)

solve :: [(Int, Int, Int)]
solve = dedup . concat
   $ arithmeticSeq
  <$> filter (`Set.member` fourDigitPrimes)
  <$> dedup . permuteInt
  <$> Set.toList fourDigitPrimes
  where
    fourDigitPrimes = Set.fromList $ takeWhile (< 10000) $ dropWhile (< 1000) $ map fromInteger primes
    permuteInt :: Int -> [Int]
    permuteInt = map read . permutations . show
    dedup :: Ord a => [a] -> [a]
    dedup = map head . group . sort
    arithmeticSeq :: (Num c, Ord c) => [c] -> [(c, c, c)]
    arithmeticSeq xs = [(x, y, z) | x <- xs, y <- xs, z <- xs, x /= y, x /= z, y /= z, y - x == z - y, y - x > 0]

main :: IO ()
main = putStrLn $ maybe "Nothing" showTuple $ find (/= (1487, 4817, 8147)) solve
  where 
    showTuple (x, y, z) = intercalate "" $ map show [x, y, z]

