module BlackJack where
import Cards
import RunGame

import Test.QuickCheck

{- A -}

{- Task 3.2
size hand2
  = size (Add (Card (Numeric 2) Hearts)
         (Add (Card  Jack       Spades) Empty))
  = 1 + size (Add (Card  Jack       Spades) Empty)
  = 1 + 1 + size Empty
  = 1 + 1 + 0
  = 2
-}

-- | Returns an empty hand
empty :: Hand
empty = Empty

-- | Property: an empty hand is empty
prop_empty :: Bool
prop_empty = Empty == empty

-- | The Integer value of a Rank
valueRank :: Rank -> Integer
valueRank (Numeric n) | n < 2       = 2 
                      | n > 10      = 10
                      | otherwise   = n
valueRank Ace                       = 11
valueRank _                         = 10

-- | Property: value of Rank is >= 2 and <= 11
prop_valueTwoEleven :: Rank -> Bool
prop_valueTwoEleven r = (v <= 11) && (v >= 2)
  where v = valueRank r

-- | The Integer value of a Card
valueCard :: Card -> Integer
valueCard (Card r _) = valueRank r

-- | Number of Aces in a hand
numberOfAces :: Hand -> Integer
numberOfAces Empty                = 0
numberOfAces (Add (Card Ace _) h) = 1 + numberOfAces h
numberOfAces (Add _            h) = numberOfAces h

-- | Property: number of aces <= than number of cards
prop_acesFewer :: Hand -> Bool
prop_acesFewer h = (numberOfAces h <= size h) && (numberOfAces h >= 0)

-- | Value of hand where Ace counts as 11
value' :: Hand -> Integer
value' Empty     = 0
value' (Add c h) = valueCard c + value' h  

-- | Value of hand
value :: Hand -> Integer
value h | v > 21    = v - n * 10
        | otherwise = v
    where
      n = numberOfAces h
      v = value' h

-- | Property: the value of a hand is less than the value with maxed Aces
prop_valueAndAces :: Hand -> Bool
prop_valueAndAces h | n == 0    = value h == value' h
                    | n == 1    = value h <= value' h
                    | otherwise = value h <  value' h 
    where n = numberOfAces h
      
-- | Is the player bust
gameOver :: Hand -> Bool
gameOver h = value h > 21

-- | Given a Guest and a Bank which has won
winner :: Hand -> Hand -> Player
winner guest bank
           | gameOver guest           = Bank
           | gameOver bank            = Guest
           | value guest > value bank = Guest
           | otherwise                = Bank   

-- | Property: Given equally valued hands the bank allways wins 
prop_bankWinEqual :: Hand -> Hand -> Property
prop_bankWinEqual guest bank = value guest == value bank ==>
                               winner guest bank == Bank 

-- | Property: If the player busts the bank wins
prop_bankWinBust :: Hand -> Hand -> Property
prop_bankWinBust guest bank = gameOver guest ==> winner guest bank == Bank
