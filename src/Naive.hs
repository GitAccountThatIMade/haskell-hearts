-- | Write a report describing your design and strategy here.
module Naive (
    playCard,
    makeBid,
    rngFromCards,
    parseMemoryRNG
)
where

-- You can add more imports as you need them.
import Hearts.Types
import Cards
import Data.Char



rngFromCards :: [Card] -> Int -> Int -- Returns a number from 0 to 100, using the given cards as a source of entropy
-- Along with some integer to multiply by (the caller should keep track to avoid repeating the same number)
rngFromCards cards coefficient =  mod (base * coefficient) 101 where
    seed = foldl (\acc card -> show card ++ acc) "" cards
    base = read (foldl (\acc int -> show int ++ acc) "" (map ord seed)) :: Int



-- checkEarly :: String -> [Card] -> Bool -- Checks if we should treat the current turn as "Early on"
-- checkEarly memory cards = rngFromCards cards turn > turn * (100 `div` parseMemoryNumPlayers memory) where -- Basically, reduce
-- -- the likelihood of "early" each round, BUT make it depend on the number of players (as more players means cards are 
-- -- used faster)
--     turn = parseMemoryRNG memory


checkBroken :: [(Card, PlayerId)] -> [(Card, PlayerId)] -> String -> Bool --Checks if hearts have been played before
-- check both the current field AND in memory AND the previous round to see if they've already been broken
checkBroken field past memory = hearts field  || queen field || hearts past || queen past || parseMemoryBroken memory where
    hearts = any (checkSuit Heart . fst )
    queen =  any ((\card -> show card == "SQ") . fst) 

constructMemory :: String  -> [(Card, PlayerId)] -> [(Card, PlayerId)] -> String -- Take the memory string and update it to what we want
-- Update the turn counter.  Always delineate with "RNG" and "|"
-- Construct the number of players from the length of the previous state
-- Construct the memory of if hearts have been broken, by checking if hearts have been played, but also if the next card played will break hearts
constructMemory memory currentPlay previousPlay  = 
    "RNG" ++ show (1 + parseMemoryRNG memory) ++ "|" 
 ++ "NUMP" ++ show (length previousPlay)
 ++ "BRO" ++ show (checkBroken currentPlay previousPlay memory) ++ "|"

-- the parse functions takes inspiration from the below site (Stack Overflow, of course) in recognising substrings
-- https://stackoverflow.com/questions/48492142/replace-a-substring-from-a-string-in-haskell
parseMemoryRNG :: String -> Int
parseMemoryRNG ('R' : 'N' : 'G' : rest) = read (parseUntilBreak "" rest) :: Int -- If RNG is found, get the number there
parseMemoryRNG (_:rest) = parseMemoryRNG rest -- keep going through the string
parseMemoryRNG "" = 1 -- return 1 as a default base case

-- parseMemoryNumPlayers :: String -> Int
-- -- Get the number of players from memory.  By default here, it is 1
-- parseMemoryNumPlayers "" = 1
-- parseMemoryNumPlayers ('N' : 'U' : 'M' : 'P' : rest) = read (parseUntilBreak "" rest)
-- parseMemoryNumPlayers (_ : rest) = parseMemoryNumPlayers rest

parseMemoryBroken :: String -> Bool
-- Find "BRO" in the memory string and return what it represents (up to "|").  Default to false
parseMemoryBroken "" = False 
parseMemoryBroken ('B' : 'R' : 'O' : rest) = parseUntilBreak "" rest == "True" 
parseMemoryBroken (_ : rest) = parseMemoryBroken rest 

parseUntilBreak :: String -> String ->  String -- goes through a string until a "|" is hit.  
parseUntilBreak _ "" = ""
parseUntilBreak current ('|' : _) = reverse current -- Will be backwards due to recursion, so put it back in order
parseUntilBreak current (top:rest) = parseUntilBreak (top : current) rest

checkCard :: String -> [Card] -> Bool
checkCard _ [] = False
checkCard cardName deck = any (\currentCard -> show currentCard == cardName) deck -- If the searched card is not
-- present, then the filtered list is going to be empty.  This just checks if the written name of the card matched that 

checkSuit :: Suit -> Card -> Bool --Returns true if a card of that suit is in the given card
--checkSuit _ [] = False
checkSuit suit (Card thisSuit _ ) = suit == thisSuit
    --any (\card -> getSuit card == suit) cards


getCard :: String -> [Card] -> Card  -- Will error if card doesn't exist, so call checkCard beforehand
getCard cardName myCards = head filtered
    where 
        filtered = filter (\n -> show n == cardName) myCards

getSuit :: Card -> Suit --Simply unpacks the suit of the card
getSuit (Card suit _ ) = suit

-- getRank :: Card -> Rank
-- getRank (Card _ rank ) = rank

-- highOfSuit :: Suit -> [Card] -> Card
-- -- Returns the HIGHEST value card of a particular suit.  If that suit isn't there, return the HIGHEST card
-- highOfSuit suit cards = if not (null suitCards) then maximum suitCards else maximum cards where
--     suitCards = filter ( (==) suit . getSuit ) cards

-- lowOfSuit :: Suit -> [Card] -> Card
-- -- Returns the LOWEST value card of a particular suit.  If that suit isn't there, return the LOWEST card
-- lowOfSuit suit cards =  if not (null suitCards) then minimum suitCards else minimum cards where
--     suitCards = filter ( (==) suit . getSuit ) cards

-- highestLegal :: [Card] -> [(Card, PlayerId)] -> [(Card, PlayerId)] -> String -> Card
-- highestLegal hand currentPool pastPool memory = foldl (\acc element -> if getRank acc >= getRank element then acc else element ) (Card Spade Two) checkedHand  
-- -- Loop through the cards and take the highest value.  The two of spades is chosen as default as if there is at least
-- -- one card in the hand, the ">=" will override it.
--     where 
--         -- Filter the point cards out UNLESS hearts have been broken
--         checkedHand = if checkBroken currentPool pastPool memory then hand else filter (\card -> getSuit card == Heart || card == Card Spade Queen) hand

-- lowestLegal :: [Card] -> [(Card, PlayerId)] -> [(Card, PlayerId)] -> String -> Card
-- lowestLegal hand currentPool pastPool memory = foldl (\acc element -> if getRank acc <= getRank element then acc else element ) (Card Spade Ace) checkedHand  
-- -- Loop through the cards and take the lowest value.  The ace of spades is chosen as default as if there is at least
-- -- one card in the hand, the "<=" will override it.
--     where
--         checkedHand = if checkBroken currentPool pastPool memory then hand else filter (\card -> getSuit card == Heart || card == Card Spade Queen) hand



compareSuit :: Card -> Card -> Bool
compareSuit card1 card2 = getSuit card1 == getSuit card2


playCard :: PlayFunc
-- Always play 2 of clubs
playCard _ myCards [] _  = if checkCard "C2" myCards then  (getCard "C2" myCards, "RNG1|NUMP1|BROFalse|") else (minimum myCards, "")  
-- Play the lowest card in the leading suit, else play the lowest card.  Also set the default memory values
playCard _ myCards playList (Just (states, memory))  = if any  (compareSuit (fst (last playList)))  myCards
    then (minimum (filter (compareSuit (fst ( last playList))) myCards), constructMemory memory playList states)
    else (minimum myCards, constructMemory memory playList states) 

playCard _ myCards playList _ = if any  (compareSuit (fst (last playList)))  myCards -- covers the no memory case to avoid warnings (Because they're errors here)
        then (minimum (filter (compareSuit (fst ( last playList))) myCards), "")
        else (minimum myCards, "") 

-- | Not used, do not remove.
makeBid :: BidFunc
makeBid = undefined

