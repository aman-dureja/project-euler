-- Problem:
--
-- By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
-- What is the 10 001st prime number?
--
-- Answer: 104743
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

main :: IO ()
main = do
  print $ "The 10 001st prime number is: "
  print $ primes !! 10000
