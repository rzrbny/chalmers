module Ex3 where

import Test.QuickCheck
import Data.List

{- 0 -}

drop' :: Int -> [a] -> [a]
drop' 0 xs     = xs
drop' _ []     = []
drop' n (x:xs) = drop (n-1) xs

prop_drop :: Int -> [Int] -> Bool
prop_drop n xs = drop' n' xs == drop n' xs
  where n' = abs n

splitAt' :: Int -> [a] -> ([a],[a])
splitAt' 0 xs     = ([],xs)
splitAt' _ []     = ([],[])
splitAt' n (x:xs) = (x:ys,zs)
  where (ys,zs) = splitAt' (n-1) xs

prop_splitAt :: Int -> [Int] -> Bool
prop_splitAt n xs = splitAt' m xs == splitAt m xs
  where m = abs n

zip3' :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3' (x:xs) (y:ys) (z:zs) = (x,y,z) : zip3 xs ys zs
zip3' _      _      _      = []

zip3'' :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3'' x y z = [(a,b,c) | ((a,b),c) <- zip (zip x y) z]

{- 1 -}

remove :: Eq a => a -> [a] -> [a]
remove _  []     = []
remove x' (x:xs) | x' == x   = xs
                 | otherwise = x:remove x' xs

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation _  [] = False
isPermutation [] _  = False
isPermutation (x:xs) ys = isPermutation xs (remove x ys)

{- 3 -}
pascal :: Int -> [Int]
pascal 0 = [1]
pascal n = [1] ++ zipWith (+) p (tail p) ++ [1]
  where p = pascal (n-1)

{- 4 -}
crossOut :: Int -> [Int] -> [Int]
crossOut m ns = [n | n <- ns , mod n m /= 0]

sieve :: [Int] -> [Int]
sieve []   = []
sieve (n:ns) = n:sieve (crossOut n ns)

{- 5 -}
isPrime :: Int -> Bool
isPrime n = elem n (sieve [2..100])

isPrimeSum :: Int -> Bool
isPrimeSum n = or [elem (n-m) ps | m <- ps]
  where ps = sieve [2..min n 100]

primeHype :: Bool
primeHype = and [isPrimeSum n | n <- [4..100], even n]

{- 6 -}
occursIn :: Eq a => a -> [a] -> Bool
occursIn x xs = or [x==x' | x' <- xs]

allOccurIn :: Eq a => [a] -> [a] -> Bool
allOccurIn xs ys = and [occursIn x ys | x <- xs]

sameElements :: Eq a => [a] -> [a] -> Bool
sameElements xs ys = allOccurIn xs ys && allOccurIn ys xs

numOccurences :: Eq a => a -> [a] -> Int
numOccurences x xs = length [x | x' <- xs, x == x']

--bag :: Eq a => [a] -> [(a,Int)]
--bag []     = []
--bag (x:[]) = [(x,1)]
--bag (x:xs) = (x,1 + numOccurences x xs) : bag (remove x xs)

{- 7 -}

positions :: [a] -> [(a,Integer)]
positions xs = zip xs [0..]

firstPosition :: Eq a => a -> [a] -> Integer
firstPosition x xs = head [n | (y,n) <- positions xs, y==x]

remove1st :: Eq a => a -> [a] -> [a]
remove1st x xs = [y | (y,n) <- positions xs, not (y == x && n == 1)]

remove' :: Eq a => Integer -> a -> [a] -> [a]
remove' m x xs = [y | (y,n) <- positions xs, not (y == x && n <= m)]

