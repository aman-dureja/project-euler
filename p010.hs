-- Problem:
--
-- The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
-- Find the sum of all the primes below two million.
--
-- Answer: 142913828922
--

primes :: [Int]
primes = 2 : filter (null . tail . primeFactors) [3,5..]

primeFactors :: Int -> [Int]
primeFactors n = factors n primes
  where
    factors n (p:ps)
      | p*p > n           = [n]
      | n `rem` p == 0    = p : factors (n `div` p) (p:ps)
      | otherwise         = factors n ps

sumAllPrimesBelow :: Int -> Int
sumAllPrimesBelow n = sum . takeWhile (<n) $ primes

main :: IO ()
main = do
  print $ "The sum of all primes below two million is: "
  print $ sumAllPrimesBelow 2000000
