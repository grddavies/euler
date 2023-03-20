-- The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.
-- Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000
import Control.Monad (join)

powerSeries :: Integral b => b -> [b]
powerSeries n = map (join (^)) [1..n]

main :: IO ()
main = print $ sum (powerSeries 1000) `mod` (10 ^ 10)

