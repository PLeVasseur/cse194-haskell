module CreditCard
( toDigits
, toDigitsRev
, doubleEveryOther
) where

-- Exercise 1
toDigits :: Integer -> [Integer]
toDigits int = reverse $ toDigitsRev int

toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev int = (int `mod` 10):toDigitsRev (int `div` 10)

-- Exercise 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther digitList = foldl  (\accumList digit ->
                                      []
                                    ) [] (reverse digitList)
