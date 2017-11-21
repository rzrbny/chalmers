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
Empty <+ hand2 = hand2
(Add card hand) <+ hand2 = Add card (hand <+ hand2)

-- | Appending hands is an associative operation
prop_onTopOf_assoc :: Hand -> Hand -> Hand  -> Bool
prop_onTopOf_assoc p1 p2 p3 =
  p1<+(p2<+p3)==(p1<+p2)<+p3

-- | Property: size of the combined hand should be the sum of the sizes
-- | of the two individual hands
prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf p1 p2 =
    (size p1 + size p2) == size (p1 <+ p2)

-- | Generates a hand with a full deck of 52 cards
fullDeck :: Hand
fullDeck = foldr Add Empty cards
  where
    ranks = [Ace,King,Queen,Jack] ++ [Numeric n | n <- [10,9..2]]
    suits = [Hearts,Spades,Diamonds,Clubs]
    cards = [Card r s | s <- suits, r <- ranks]

-- | Property: any "valid" card is a part of the full deck
prop_fullDeckFull :: Card -> Bool
prop_fullDeckFull (Card (Numeric n) s) = (n >=2 && n <= 10) ==
                                         belongsTo (Card (Numeric n) s)
                                          fullDeck
prop_fullDeckFull c                    = belongsTo c fullDeck

-- | Property: a full deck contains 52 cards
prop_full :: Bool
prop_full = size fullDeck == 52

-- | Add the top card of the second hand to the first
draw :: Hand -> Hand -> (Hand, Hand)
draw Empty     h = error "draw: The deck is empty."
draw (Add c d) h = (d, Add c h)

-- | Property: drawing a card preserves the total number of cards
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

-- | Property: given a deck with value >= 16 the bank will score >= 16
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

-- | Property: shuffling preserves deck size
prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle g d = size (shuffle g d) == size d

-- | Property: shuffling doesn't change the cards in the deck
prop_shuffleSameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffleSameCards g c d = belongsTo c d == belongsTo c (shuffle g d)

-- | Removes nth card from a deck and returns deck and card
remove :: Integer -> Hand -> (Card,Hand)
remove 0 (Add c d) = (c,d)
remove n (Add c d) = (c',Add c d')
  where (c',d') = remove (n-1) d

-- | Property: remove decreases size of deck by 1
prop_removeDecr :: Integer -> Hand -> Property
prop_removeDecr n h = m < size h && h /= Empty ==> size d + 1 == size h
  where
    m = abs n
    (c,d) = remove m h

-- | Returns True if a card belongs to a given deck
belongsTo :: Card -> Hand -> Bool
belongsTo c Empty      = False
belongsTo c (Add c' d) = c == c' || belongsTo c d

{- The following functions are for testing that shuffle produces a deck of
   random order. We express this as the fact that a shuffled deck should
   not contain sequences of cards similar to the unshuffled deck. We can
   calculate that even a sequence of 4 cards appearing in both is highly
   unlikely, at least in the order of e-4. -}

-- | Checks if the first deck is the top of the second deck
isTop :: Hand -> Hand -> Bool
isTop Empty h                  = True
isTop d     Empty              = False
isTop (Add c1 h1) (Add c2  h2) = (c1 == c2) && isTop h1 h2

-- | Checks if a sequence of cards is contained in a deck
isIn :: Hand -> Hand -> Bool
isIn h Empty = False
isIn h d     | size h <= size d = isTop h d || isTop h d'
             | otherwise        = False
  where (Add c d') = d

-- | Copies the n first cards of a deck into a new deck
copyCards :: Integer -> Hand -> Hand
copyCards 0 h         = Empty
copyCards n (Add c h) = Add c (copyCards (n-1) h)

-- | Checks if there exists a sequence of n cards in the first deck
--   that is also a sequence in the second deck
hasSequence :: Integer -> Hand -> Hand -> Bool
hasSequence n h d | size h == n = isIn (copyCards n h) d
                  | size h >  n = isIn (copyCards n h) d ||
                    hasSequence n h' d
                  | otherwise   = False
  where Add _ h' = h

-- | Checks if a a sequence of length n in a shuffled full deck is contained
--   in the unshuffled deck
shuffleShuffles ::  StdGen -> Integer -> Bool
shuffleShuffles g n = not (hasSequence n (shuffle g fullDeck) fullDeck)

-- | The odds of a shuffled deck containing a sequence of 5 cards from the
--   unshuffled full deck is almost zero
prop_shuffleShuffles4 :: StdGen -> Bool
prop_shuffleShuffles4 g = shuffleShuffles g 4
