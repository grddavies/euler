-- It was proposed by Christian Goldbach that every odd composite number can be written as the sum of a prime and twice a square.
-- It turns out that the conjecture was false.
-- What is the smallest odd composite that cannot be written as the sum of a prime and twice a square?

import Data.Foldable (find)
import qualified Data.Map as M
import qualified Control.Monad
import Data.Maybe (fromJust)
import Primes (primes)

main :: IO ()
main = putStrLn $ maybe "Goldbach was right!" show (find notGoldbachs odds)

odds :: [Integer]
odds = [3, 5 ..]

member :: Ord a => a -> [a] -> Bool
member x xs = x `elem` takeWhile (<= x) xs

notGoldbachs :: Integer -> Bool
notGoldbachs = Control.Monad.ap ((&&) . not . goldbachs) (not . (`member` primes))

goldbachs :: Integer -> Bool
goldbachs x = any (`member` primes) remainders where
  remainders = map (x - ) $ takeWhile (< x) doubledSquares
  doubledSquares = map ((2 *) . (^ 2)) [1..]
