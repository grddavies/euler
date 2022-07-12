module Primes where

import qualified Data.Map as M

primes :: [Integer]
primes = sieve [2 ..]


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