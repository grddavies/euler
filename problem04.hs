--  A palindromic number reads the same both ways. The largest palindrome made
--  from the product of two 2-digit numbers is 9009 = 91 × 99.
--  Find the largest palindrome made from the product of two 3-digit numbers.
import Data.List (find, sortBy, tails)
import Data.Maybe (fromJust)

isPalindromic :: Int -> Bool
isPalindromic n = n == (read (reverse (show n)) :: Int)

generateProducts :: Num a => [a] -> [a]
generateProducts xs = [x * y | (x:ys) <- tails xs, y <- ys]

sortDesc :: [Int] -> [Int]
sortDesc = sortBy (flip compare)

main :: IO ()
main = print . fromJust $ find isPalindromic . sortDesc $ generateProducts threeDigits
  where
    threeDigits = [999, 998 .. 100]