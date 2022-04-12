import Data.List (foldl')
--    What is the smallest positive number that is evenly divisible by all of
--    the numbers from 1 to 20?

-- LCM of ab is a*b/HCF
lowestCommonMultiple a b = (a * b) `quot` highestCommonFactor a b

-- HCF of ab, b > a, also divides b-a => HCF ab = HCF b, a mod b
highestCommonFactor a 0 = a
highestCommonFactor a b = highestCommonFactor b (a `mod` b)


main :: IO ()
main = print $ foldl' lowestCommonMultiple 1 [2..20]