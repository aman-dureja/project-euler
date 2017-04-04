-- Problem:
--
--The prime factors of 13195 are 5, 7, 13 and 29.
--What is the largest prime factor of the number 600851475143 ?
--

listOfPrimes :: [Int]
listOfPrimes = 2 : [ x | x <- [3,5..], null (tail $ primeFactors x) ]

primeFactors :: Int -> [Int]
primeFactors n = getPrimeFactors n listOfPrimes
  where
    getPrimeFactors n (p:ps)
      | p*p > n           = [n]
      | n `rem` p == 0    = p : getPrimeFactors (n `div` p) (p:ps)
      | otherwise         = getPrimeFactors n ps

largestPrimeFactor :: Int -> Int
largestPrimeFactor = last . primeFactors

main :: IO ()
main = do
  print $ "The largest prime factor of the number 600851475143 is: "
  print $ largestPrimeFactor 600851475143
