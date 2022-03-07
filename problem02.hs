-- By considering the terms in the Fibonacci sequence whose values do not
-- exceed four million, find the sum of the even-valued terms.
fib :: Int -> Integer
fib 1 = 1
fib 2 = 2
fib x = fib (x - 2) + fib (x - 1)

fibSeq :: [Integer]
fibSeq = map fib [1..]

main :: IO()
main = putStr . show $ sum $ filter even $ takeWhile ( < 4000000) fibSeq
