-- Find the sum of all the multiples of 3 or 5 below 1000.
main :: IO ()
main = print $ sumMultiplesBelow 3 5 1000

sumArith :: (Integral a) => a -> a -> a -> a
sumArith a d n = n * (2 * a + (n - 1) * d) `div` 2

sumMultiplesBelow :: Integer -> Integer -> Integer -> Integer
sumMultiplesBelow x y lim = sumX + sumY - sumLcm
 where
  sumX = sumBelowLim x 1000
  sumY = sumBelowLim y 1000
  sumLcm = sumBelowLim (lcm x y) 1000
  sumBelowLim n lim = sumArith n n ((lim - 1) `div` n)
