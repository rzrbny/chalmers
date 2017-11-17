import Data.List (sort)
import System.IO (hFlush,stdout)
import Prelude hiding (repeat)
import Test.QuickCheck

{- 0 -}

prompt p = do putStr p
              hFlush stdout

mainA :: IO ()
mainA = do
  putStrLn "Enter integer: "
  s  <- getLine
  ns <- getInputs (read s)
  putStrLn ("Sum: " ++ show (foldr (\x -> (+) (read x)) 0 ns))

getInputs :: Integer -> IO [String]
getInputs 0 = return [] 
getInputs n = do putStrLn ("Number :")
                 s <- getLine
                 ss <- getInputs (n-1)
                 return (s:ss)

sumOfNumbers =
  do putStrLn "Compute some numbers."
     prompt "Number of numbers: "
     n <- readLn
     putStr n

mainB :: IO ()
mainB = do ns <- getInts
           putStrLn (show (sort ns))
           

getInts :: IO [Integer]
getInts = do s <- getLine
             nextInt (read s)
  where nextInt 0 = return [0]
        nextInt n = do ss <- getInts
                       return (n:ss)

repeat :: IO Bool -> IO () -> IO ()
repeat test op = do op
                    b <- test
                    if b then return () else repeat test op

{- 1 -}
--prop_LookNothing :: (Eq a, Eq b) => a -> [(a,b)] -> Property
prop_LookNothing :: Integer -> [(Integer,String)] -> Property
prop_LookNothing x xys = lookup x xys == Nothing ==> not (elem x (fst (unzip xys)))

prop_LookJust :: Integer -> [(Integer,String)] -> Property
prop_LookJust x xys = lookup x xys == Just y ==> elem (x,y) xys
  where Just y = lookup x xys

prop_Look :: Integer -> [(Integer,String)] -> Property
prop_Look x xys = prop_LookJust x xys .||. prop_LookNothing x xys 
