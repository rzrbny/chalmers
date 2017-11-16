module Ex2 where

import Test.QuickCheck


{- 1 -}
max' :: Ord a => a -> a -> a
max' x y | x > y     = x
         | otherwise = y

prop_maxSucc :: (Enum a, Ord a) => a -> Bool
prop_maxSucc x = max' x (succ x) == (succ x)

prop_maxBigger :: (Ord a) => a -> a -> Bool
prop_maxBigger x y | x > y     = max' x y == x
                   | otherwise = max' x y == y

{- 2 -}
sumsq :: Integer -> Integer
sumsq 0 = 0;
sumsq n = n*n + sumsq (n-1)

prop_sumsqExp :: Integer -> Bool
prop_sumsqExp n =  sumsq n' == div (n'*(n'+1)*(2*n'+1)) 6
  where n' = abs n

{- 3 -}
hanoi :: Integer -> Integer
hanoi 1 = 1
hanoi n = hanoi (n-1) * 2 + 1

hanoi4 :: Integer -> Integer
hanoi4 1 = 1
hanoi4 2 = 3
hanoi4 n = hanoi (n-2) * 2 + 3

hanoiDiff :: Integer -> Integer
hanoiDiff n = (hanoi n) - (hanoi4 n)

hanoiRatio :: Integer -> Double
hanoiRatio n = (fromIntegral (hanoi n)) / (fromIntegral (hanoi4 n))

{- 4 -}
fib :: Integer -> Integer
fib 0 = 1;
fib 1 = 1;
fib n = fib (n-1) + fib (n-2)

fibList :: Integer -> [Integer]
fibList 0 = [1]
fibList n = fib n : fibList (n-1)

fibAux :: Integer -> Integer -> Integer -> Integer
fibAux 0 n m = n
fibAux 1 n m = m
fibAux k n m = fibAux (k-1) m (n + m)

fib' :: Integer -> Integer
fib' n = fibAux n 1 1

prop_fibEq :: Integer -> Bool
prop_fibEq n' = fib n == fib' n
  where n = abs n'

prop_fibAux :: Integer -> Integer -> Bool
prop_fibAux n k = fibAux 0 (fib n') (fib (n'+1)) == fib (n'+0)
  where n' = abs n
        k' = abs k

{- 5 -}
smallestFactor :: Integer -> Integer
smallestFactor 1 = 1
smallestFactor n = nextFactor n 2
  
nextFactor :: Integer -> Integer -> Integer
nextFactor n k | r == 0    = k
               | otherwise = nextFactor n (k+1)
  where r = mod n k
        
prop_factorLeq :: Integer -> Bool
prop_factorLeq n' = smallestFactor n <= n
  where n = abs n' + 1

prop_factorCorr :: Integer -> Bool
prop_factorCorr n' = mod n (smallestFactor n) == 0
  where n = abs n' + 1

numFactors :: Integer -> Int
numFactors n = length [m | m <- [1..n], mod n m == 0]

{- 6 -}
multiply :: Num a => [a] -> a
multiply []   = 1
multiply (x:xs) = x * multiply xs

{- 7 -}
duplicates :: Eq a => [a] -> Bool
duplicates [] = False
duplicates (x:xs) = elem x xs || duplicates xs



removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) | elem x xs = removeDuplicates xs
                        | otherwise = x : removeDuplicates xs

prop_duplicatesRemoved :: [Integer] -> Bool
prop_duplicatesRemoved xs = not (duplicates (removeDuplicates xs))

{- 8 -}
data Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec
             deriving (Eq, Show, Enum, Ord)

daysInMonth :: Month -> Integer -> Integer
daysInMonth Feb y | mod y 4 == 0 = 29
                  | otherwise     = 28
daysInMonth Apr _ = 30
daysInMonth Jun _ = 30
daysInMonth Sep _ = 30
daysInMonth Nov _ = 30
daysInMonth _   _ = 31

data Date = Date Integer Month Integer
            deriving (Show)
validDate :: Date -> Bool
validDate (Date y m d) = d > 0 && d <= daysInMonth m y

nextMonth :: Month -> Month
nextMonth Dec = Jan
nextMonth m   = succ m

tomorrow :: Date -> Date
tomorrow (Date y Dec d) | d == daysInMonth Dec y = Date (y+1) Jan 1
tomorrow (Date y m d)   | d == daysInMonth m   y = Date y (succ m) 1
                        | otherwise              = Date y m (d+1)

