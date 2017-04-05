-- Problem:
--
-- 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
-- What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
--
-- Answer: 232792560
--

import Data.List

primes :: [Int]
primes = 2 : filter (null . tail . primeFactors) [3,5..]

primeFactors :: Int -> [Int]
primeFactors n = factors n primes
  where
    factors n (p:ps)
      | p*p > n           = [n]
      | n `rem` p == 0    = p : factors (n `div` p) (p:ps)
      | otherwise         = factors n ps

smallestEvenlyDivisble :: Int -> [Int] -> Int
smallestEvenlyDivisble 1 xs = product xs
smallestEvenlyDivisble n xs =
  smallestEvenlyDivisble (n - 1) primeDifference
  where primeDifference = xs ++ (primeFactors n \\ xs)

main :: IO ()
main = do
  print $ "The smallest positive number evenly divisible by all the numbers from 1 to 20 is: "
  print $ smallestEvenlyDivisble 20 []
