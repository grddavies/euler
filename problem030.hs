-- Surprisingly there are only three numbers that can be written as the sum
-- of fourth powers of their digits:
--
--  1634 = 1^4 + 6^4 + 3^4 + 4^4
--  8208 = 8^4 + 2^4 + 0^4 + 8^4
--  9474 = 9^4 + 4^4 + 7^4 + 4^4
--
-- As 1 = 1^4 is not a sum it is not included.
--
-- The sum of these numbers is 1634 + 8208 + 9474 = 19316.
--
-- Find the sum of all the numbers that can be written as the sum of fifth
-- powers of their digits.
import Data.Char (digitToInt)
import Data.List (find)

nthPowers :: Integer -> [Integer]
nthPowers n = [x ^ n | x <- [0..9]]

-- point where (nines n) > 9 ^ 5 * n
-- NOTE: with zeros, this is not actually an upper limit...
upperLimit = find (\x -> (nines x) > (x * (9 ^ 5))) [1..]
  where
    nines :: Int -> Int
    nines n = read $ take n $ repeat '9'

sumOfDigitPowers n = [x | x <- [2..999999], x == (sum $ map ((^ n) . digitToInt) (show x))]

main :: IO ()
main = putStrLn . show . sum $ sumOfDigitPowers 5
