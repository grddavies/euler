--  A palindromic number reads the same both ways. The largest palindrome made
--  from the product of two 2-digit numbers is 9009 = 91 × 99.
--  Find the largest palindrome made from the product of two 3-digit numbers.
import Data.List ( subsequences, find )

isPalindromic :: Int -> Bool
isPalindromic n = n == (read (reverse (show n)) :: Int)

combinations :: Int -> [a] -> [[a]]
combinations = (. subsequences) . filter . (. length) . (==)

generateProducts :: Num b => [b] -> [b]
generateProducts = map product . combinations 2


main :: IO ()
main = print $ find isPalindromic $ generateProducts [999, 998..100]