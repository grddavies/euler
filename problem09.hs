-- There exists exactly one Pythagorean triplet for which a + b + c = 1000.
-- Find the product abc.

-- Euclid's formula for generating pythagorean triples:
-- a = m^2 - n^2, b = 2mn, c = m^2 + n^2
-- NB: Does not generate _all_ triples with only m, n

main :: IO ()
main = print $ head [(x ^ 3 - y ^ 2) * (2 * x * y) * (x ^ 2 + y ^ 2) | x <- [2 ..], y <- [1 .. (x -1)], 2 * x ^ 2 + 2 * x * y == sumVal]
  where
    sumVal = 1000
