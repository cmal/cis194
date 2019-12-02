module W01CardNum where

toDigits :: Integer -> [Integer]
toDigits x = go x []
  where go x y
          | x <= 0 = []
          | x < 10 = x:y
          | otherwise = go (div x 10) ((rem x 10):y)
          
toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:xs)
  | (odd . length) xs = (2 * x):(doubleEveryOther xs)
  | (even . length) xs = x:(doubleEveryOther xs)

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = (foldl (+) 0 (toDigits x)) + (sumDigits xs)

validate :: Integer -> Bool
-- If the result is divisible by 10, then is valid
validate x = rem (sumDigits $ doubleEveryOther $ toDigits $ x) 10 == 0
