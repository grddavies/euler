-- What is the largest prime factor of the number 600851475143 ?
main :: IO()
main = putStr . show $ findLargestPrimeFactor 600851475143

primes :: [Integer]
primes = seive [2..]
seive :: Integral a => [a] -> [a]
seive (p:xs) = p : seive [x | x <- xs, x `mod` p > 0]
seive [] = []

findLargestPrimeFactor :: Integer -> Integer
findLargestPrimeFactor n = go n primes
    where go 1 ps = head ps
          go n (p:ps) = if n `mod` p == 0 then go (n `div` p) (p:ps) else go n ps
          go n [] = error "Unreachable - primes is infinite list"
