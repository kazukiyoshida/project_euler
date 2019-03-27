-- 1
-- If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9.
-- The sum of these multiples is 23.
-- Find the sum of all the multiples of 3 or 5 below 1000.

sumOfThreeOrFiveMultTo :: Integer -> Integer
sumOfThreeOrFiveMultTo n
  | n <= 0    = 0
  | otherwise = sum $ filter fiveOrThreeMult [1..n-1]

fiveOrThreeMult :: Integer -> Bool
fiveOrThreeMult n
  | n <= 0         = False
  | n `mod` 3 == 0 = True
  | n `mod` 5 == 0 = True
  | otherwise      = False

main = do
  print $ "the answer is "
  print $ sumOfThreeOrFiveMultTo 10
  print $ sumOfThreeOrFiveMultTo 1000

  let isThreeOrFiveMult n = (n `mod` 3) == 0 || (n `mod` 5) == 0
  print $ sum $ filter isThreeOrFiveMult [1..9]
  print $ sum $ filter isThreeOrFiveMult [1..999]
