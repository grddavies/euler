-- Longest Collatz sequence starting below 1 million

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (maybe)

reverseSecond :: [(a, b)] -> [(a, b)]
reverseSecond xs =
  let (as, bs) = unzip xs
      bs' = reverse bs
  in zip as bs'

addConstantToSecond :: (Functor f, Num b) => b -> f (a, b) -> f (a, b)
addConstantToSecond c = fmap (\(x, y) -> (x, y + c))

collatzSequence :: (Num a, Integral k) => k -> Map k a -> Map k a
collatzSequence n m
    | n <= 0    = error "Input must be a positive integer"
    | otherwise = collatzMemo n 1 [] m
  where
    collatzMemo n len xs map = 
        case Map.lookup n map of
            Just value -> Map.fromList (addConstantToSecond value (reverseSecond xs))
            Nothing -> collatzSequence' n len xs map
    -- We reach 1 all sequence elements are new to the map
    collatzSequence' 1 steps seq _ = Map.fromList (reverseSecond ((1, steps) : seq)) `Map.union` m
    -- Otherwise we check the memoization of the next member of the sequence
    collatzSequence' n steps seq map
        | even n    = collatzMemo (n `div` 2) (steps + 1) ((n, steps) : seq) map
        | otherwise = collatzMemo (3*n + 1) (steps + 1) ((n, steps) : seq) map

getMaxesFromMap :: Ord a1 => Map a2 a1 -> Maybe a2
getMaxesFromMap m = go Nothing Nothing (Map.toList m)
  where
    go maxKey _        []           = maxKey 
    go _ Nothing  ((k,v):rest) = go (Just k) (Just v) rest
    go maxK (Just u) ((k,v):rest)
        | v < u     = go maxK       (Just u) rest
        | v > u     = go (Just k)   (Just v) rest
        | otherwise = go (Just k)     (Just v) rest

main :: IO ()
main = do
  let range = [999999, 999998..1]
  let mem = foldr (\x acc -> (collatzSequence x acc) `Map.union` acc) Map.empty range
  let startingNumbers = Map.filterWithKey (\k _ -> k < 1000000) mem
  putStrLn $ maybe "Nothing" show (getMaxesFromMap startingNumbers)
