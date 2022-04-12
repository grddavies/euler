
--  Find the difference between the sum of the squares of the first one
--  hundred natural numbers and the square of the sum

sumOfSquares :: Integral a => a -> a
sumOfSquares n = n * (n + 1) * (2 * n + 1) `quot` 6

squareOfSum :: Integral a => a -> a
squareOfSum n = n ^ 2 * (n + 1) ^ 2 `quot` 4

main :: IO()
main = print $ squareOfSum 100 - sumOfSquares 100