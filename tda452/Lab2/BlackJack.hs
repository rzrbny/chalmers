module BlackJack where
import Cards
import RunGame
import System.Random

import Test.QuickCheck hiding(shuffle)

{- Implementation -}
implementation = Interface
  { iEmpty    = empty
  , iFullDeck = fullDeck
  , iValue    = value
  , iGameOver = gameOver
  , iWinner   = winner 
  , iDraw     = draw
  , iPlayBank = playBank
  , iShuffle  = shuffle
  }

main :: IO ()
main = runGame implementation

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

{- B -}

-- | Puts a hand on top of another
(<+) :: Hand -> Hand -> Hand
(<+) Empty     h2 = h2
(<+) (Add c h) h2 = Add c (h<+h2)

-- | Appending hands is a associative operation
prop_onTopOf_assoc :: Hand -> Hand -> Hand  -> Bool
prop_onTopOf_assoc p1 p2 p3 =
  p1<+(p2<+p3)==(p1<+p2)<+p3

-- | Generates a hand with numeric cards with value lower than n of a suit
partialSuit :: Integer -> Suit -> Hand
partialSuit 2 s = Add (Card (Numeric 2) s) Empty
partialSuit n s = Add (Card (Numeric n) s) (partialSuit (n-1) s)

-- | Generates a hand with a full suit of 13 cards
fullSuit :: Suit -> Hand
fullSuit s =  Add (Card Ace   s)
             (Add (Card King  s)
             (Add (Card Queen s)
             (Add (Card Jack  s) Empty)))
             <+ partialSuit 10 s

-- | Generates a hand with a full deck of 52 cards
fullDeck :: Hand
fullDeck = fullSuit Hearts <+ fullSuit Spades <+ fullSuit Diamonds <+ fullSuit Clubs

-- | Any "valid" card is a part of the full deck
prop_fullDeckFull :: Card -> Bool
prop_fullDeckFull (Card (Numeric n) s) = (n >=2 && n <= 10) == belongsTo (Card (Numeric n) s) fullDeck
prop_fullDeckFull c                    = belongsTo c fullDeck

-- | A full deck contains 52 cards
prop_full :: Bool
prop_full = size fullDeck == 52

-- | A full deck contains 4 Aces
prop_fourAces :: Bool
prop_fourAces = numberOfAces fullDeck == 4

-- | Add the top card of the second hand to the first
draw :: Hand -> Hand -> (Hand, Hand)
draw Empty     h = error "draw: The deck is empty."
draw (Add c d) h = (d, Add c h)

-- | Drawing a card preserves the total number of cards
prop_drawTotal :: Hand -> Hand -> Bool
prop_drawTotal d h | d /= Empty = size d + size h == size d' + size h'
                   | otherwise  = True
  where (d',h') = draw d h

-- | Returns the result of the bank playing with a given deck
playBank :: Hand -> Hand
playBank d = playBank' d Empty

-- | The bank draws cards until its score is >= 16
playBank' :: Hand -> Hand -> Hand
playBank' d b | value b >= 16 = b
              | otherwise     = playBank' d' b'
  where (d',b') = draw d b

-- | Given a deck with value >= 16 the bank will score >= 16
prop_playBank16 :: Hand -> Bool
prop_playBank16 d | value d >= 16 = value (playBank d) >= 16
                  | otherwise     = True

-- | Shuffles a deck
shuffle :: StdGen -> Hand -> Hand
shuffle g Empty = Empty
shuffle g d     = Add c (shuffle g' d')
  where
    (n,g') = randomR (0,size d - 1) g
    (c,d') = remove n d

-- | Shuffling preserves size
prop_shuffleSize :: StdGen -> Hand -> Bool
prop_shuffleSize g d = size (shuffle g d) == size d

-- | Shuffling doesn't change the cards in the deck
prop_shuffleSameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffleSameCards g c d = belongsTo c d == belongsTo c (shuffle g d)

-- | Removes nth card from a deck and return deck and card
remove :: Integer -> Hand -> (Card,Hand)
remove 0 (Add c d) = (c,d)
remove n (Add c d) = (c',Add c d')
  where (c',d') = remove (n-1) d

-- | Remove decreases size of deck by 1
prop_removeDecr :: Integer -> Hand -> Property
prop_removeDecr n h = m < size h && h /= Empty ==> size d + 1 == size h
  where
    m = abs n
    (c,d) = remove m h

-- | Returns True if a card belongs to a given deck
belongsTo :: Card -> Hand -> Bool
belongsTo c Empty      = False
belongsTo c (Add c' d) = c == c' || belongsTo c d

-- | Test decks
d1 = Add (Card Ace Hearts) (
     Add (Card King Diamonds) (
     Add (Card (Numeric 5) Clubs) Empty))
