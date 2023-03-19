-- In 2004 there was found a massive non-Mersenne prime which contains 2,357,207 digits:
-- 28433×2 ^ 7830457 + 1

-- Find the last ten digits of this prime number.

p = 28433 * 2 ^ 7830457 + 1

main = print $ p `mod` 10 ^ 10

