-- It was proposed by Christian Goldbach that every odd composite number can be written as the sum of a prime and twice a square.
-- It turns out that the conjecture was false.
-- What is the smallest odd composite that cannot be written as the sum of a prime and twice a square?

import Data.Foldable (find)
import qualified Data.Map as M
import qualified Control.Monad
import Data.Maybe (fromJust)

main :: IO ()
main = putStrLn $ maybe "Goldbach was right!" show (find notGoldbachs odds)

odds :: [Integer]
odds = [3, 5 ..]

primes :: [Integer]
primes = sieve [2 ..]

member :: Ord a => a -> [a] -> Bool
member x xs = x `elem` takeWhile (<= x) xs

notGoldbachs :: Integer -> Bool
notGoldbachs = Control.Monad.ap ((&&) . not . goldbachs) (not . (`member` primes))

goldbachs :: Integer -> Bool
goldbachs x = any (`member` primes) remainders where
  remainders = map (x - ) $ takeWhile (< x) doubledSquares
  doubledSquares = map ((2 *) . (^ 2)) [1..]

-- https://doi.org/10.1017/S0956796808007004
sieve :: Integral a => [a] -> [a]
sieve xs = sieve' xs M.empty
  where
    sieve' [] table = []
    sieve' (x : xs) table = case M.lookup x table of
      Nothing -> x : sieve' xs (M.insert (x * x) [x] table)
      Just facts -> sieve' xs (foldl reinsert (M.delete x table) facts)
      where
        reinsert table prime = M.insertWith (++) (x + prime) [prime] table
