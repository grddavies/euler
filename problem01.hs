-- Find the sum of all the multiples of 3 or 5 below 1000.
main :: IO()
main = putStr . show $ sumBelowLim 3 + sumBelowLim 5 - sumBelowLim 15
-- 233168

sumArith :: Integral a => a -> a -> a -> a
sumArith a d n = n * (2*a + (n - 1)*d) `div` 2

getMax :: Integer -> Integer
getMax = div 999

sumBelowLim :: Integer -> Integer
sumBelowLim x = sumArith x x (getMax x)
