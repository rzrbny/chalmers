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

-- | An empty hand is empty
prop_empty :: Bool
prop_empty = Empty == empty

