module BlackJack where
import Cards
import RunGame

{-
size hand2
  = size (Add (Card (Numeric 2) Hearts)
         (Add (Card  Jack       Spades) Empty))
  = 1 + size (Add (Card  Jack       Spades) Empty)
  = 1 + 1 + size Empty
  = 2 + 0
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
valueRank (Numeric n) | n < 2  = 2
                      | n > 10 = 10
                      | True   = n
valueRank Ace                  = 11
valueRank _                    = 10

-- | Property: value of Rank is >= 2 and <= 11
prop_valueTwoTen :: Rank -> Bool
prop_valueTwoTen r = (v <= 11) && (v >= 2)
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

-- | Value of a Hand
value :: Hand -> Integer
value h | n == 1 = 0
    where n = numberOfAces h


