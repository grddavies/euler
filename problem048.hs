-- The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.
-- Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000
import Control.Monad (join)

powerSeries :: Integral b => b -> [b]
powerSeries = map (join (^)) . enumFrom

takeLastN :: Int -> [a] -> [a]
takeLastN n = reverse . take n . reverse

main :: IO ()
main = putStrLn . takeLastN 10 $ show $ sum $ powerSeries 1000
