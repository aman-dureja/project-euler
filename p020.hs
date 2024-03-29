{-
  Problem:

  n! means n × (n − 1) × ... × 3 × 2 × 1

  For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,
  and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.

  Find the sum of the digits in the number 100!

  Answer: 648
-}

import Data.Char

fact :: Integer -> Integer
fact n = product [2..n]

digitSum :: Integer -> Integer
digitSum n = sum [toInteger . digitToInt $ x | x <- show n]

main :: IO ()
main = do
  print $ "Find the sum of the digits in the number 100! is: "
  print $ digitSum . fact $ 100
