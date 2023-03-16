-- What is the first term in the Fibonacci sequence to contain 1000 digits?
-- Q states fibs starts 1, 1 not 0, 1

fibs :: [Integer]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

solve = head $ dropWhile (< 10 ^ 999) fibs

main :: IO ()
main = putStrLn $ show $ solve

