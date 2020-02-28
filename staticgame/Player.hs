-- | Strategy report:  IMPORTANT - PLEASE NOTE: This document has been checked, and restricted to 120 character lines.
-- |        However, it has been noted that on other IDEs, it may not be appear this way.  If this is the case, please 
-- |        contact me at enar0002@student.monash.edu for clarification.
-- | My strategy implements a heuristic-based approach to playing Hearts.  It has several strategies that it uses 
-- | depending on the different circumstances that the playCard is called under.
-- |
-- | Case 1: playCard is called at the start of the first trick of a round:
-- |        The player MUST have the Two of Clubs in this case, as otherwise, it wouldn't be the first card.
-- |        In this situation, the Two of Clubs is played.
-- | 
-- | Case 2: playCard is called in the first trick of the round, but not as the first play.
-- |        If the player has any cards of the leading suit, play the heighest card.  This is because in the first
-- |        trick, no point cards may be played, so the player is safe.
-- | 
-- |        If the player DOESN'T have a card of the leading suit, then play the lowest card.  (Note, there is an edge
-- |        case where the Queen of Spades is the lowest card that is tested for.  This would come up in a very unlikely
-- |        situation, however it was put in for the sake of robustness)
-- | 
-- | In both the above cases, the first trick is covered.  Therefore, at the end of each of cases 1 and 2, a default
-- | memory string is stored, in the format that a function can interpret later.
-- | 
-- | Case 3: playCard is called at the start of any trick after the first.
-- |        Here, the AI strategies deviate slightly between two and four player games.  This was attempted to be kept
-- |        at a minimum, but there is a clear difference in strategy and so the distinciton had to be made.
-- | 
-- |        Four Players:
-- |                Using a PSEUDO-RANDOM NUMBER GENERATOR, the algorithm determines whether or not it is early on in 
-- |                the game.  If it IS early on, play the heighest valid card.  The logic here is that winning the 
-- |                trick is unlikely to provide any points, because hearts are probably not broken yet.  this way, we 
-- |                get rid of a high card early on.
-- | 
-- |                If it isn't deemed early on, then just play the lowest valid card.
-- | 
-- |        Two Players:
-- |                Due to this situation being a perfect information game, there are a lot more specific instructions
-- |                that can be given.
-- |   
-- |                If hearts are broken, and the player has a heart in their hand, AND the player hasn't lost a trick
-- |                yet, then play the maximum heart card.  This is in order to try and reduce the chance of the 
-- |                opponent from "Shooting the Moon".
-- | 
-- |                
-- |                If the player has the Queen of spades and the opponent's only spade cards are the Ace
-- |                and/or King, then play the Queen, because they HAVE to play their high spades.
-- |                
-- |                The opponent's cards are calulated by finding the complement of: all the cards in the player's hand,
-- |                along with all the cards in the current trick, those in the last trick, AND all the cards that have 
-- |                been stored in memory.  This allows the player AI to make decisions based on perfect information.
-- | 
-- |                If the opponent has the Queen of Spades, then there are a few different strategies.  If the player
-- |                has more spades than the opponent (excluding spades higher than Queen), then play them in order to 
-- |                make the opponent play the queen and get 13 points.  The cards are played highest to lowers, in an 
-- |                attempt to let the player lead and keep forcing spades to be played.
-- |                    Failing this, play the cards in the suit that the player has least number of cards in.  If this 
-- |                is spades, then instead play the maximum, and hope the opponent's AI doesn't capitalise off of their
-- |                advantageous position.
-- |                
-- |                If the player HAS got the queen of spades, then get rid of the lowest quantity suit in the player's
-- |                hand, because this means that the opponent will have more left.  Then, when the opponent plays a 
-- |                a card and the player has none, the player can safely play the Queen of spades.
-- |
-- |                    However, if this least suit is Spades, don't play this, as it will let the opponent make us play 
-- |                the Queen of Spades.  Instead, play the lowest legal card.
-- |
-- | Case 4: playCard is called on a trick after the first, and not as the leading player for that trick.
-- |        If we CAN play a heart (hearts broken, and if we have one), and the player hasn't lost a heart yet, the 
-- |        play the maximum heart.  If we win one or two points, it stops shooting the moon.
-- | 
-- |        If we can match the leading suit, do so!  If we can, play the highest card below the current lead.  If we
-- |        can't, then play the lowest above the card to minimise the chance of losing.  Otherwise, if we're last and 
-- |        going to lose anyway, play the highest card, to get rid of a high card.
-- |
-- |        If we CAN'T match the leading suit, then play the Queen of Spades, if we have it.  It's a good opportunity 
-- |        to get rid of it with zero risk at "winning".
-- |    
-- |        And if we don't have one, then play the highest legal card, prioritising hearts, in order to drop cards that 
-- |        are more likely to make us lose.
-- |
-- |
-- | Cases 3 and 4 save a whole slew of data to the memory string.  It stores whether hearts have been broken, whether 
-- | the player has lost a round, the number of players, the coefficient for the RNG (to allow for a different "Random"
-- | number to be generated each time), and most importantly, all the cards that have been played.
-- |
-- |
-- | This is the overall strategy for the player AI.  More specifics are clarified in the function defintions below.
-- | 
module Player (
    playCard,
    makeBid,
)
where

-- You can add more imports as you need them.
import Hearts.Types
import Cards
import Data.Char
import Text.Read

-- Returns a number from 0 to 100, using the given cards as a source of entropy
-- Requires with some integer to multiply by (the caller should keep track to avoid repeating the same number)
rngFromCards :: [Card] -> Int -> Int 
rngFromCards cards coefficient =  mod (base * coefficient) 101 where
    seed = foldl (\acc card -> show card ++ acc) "" cards
    base = read (foldl (\acc int -> show int ++ acc) "" (map ord seed)) :: Int


-- Checks if we should treat the current turn as "Early on", in terms of player size.
-- Basically, reduce the likelihood of "early" each round, BUT make it depend on the number of players (as more players 
-- means cards are used faster).  This uses the RNG discussed below
checkEarly :: String -> [Card] -> Bool 
checkEarly memory cards = rngFromCards cards turn > turn * (100 `div` parseMemoryNumPlayers memory) where 
    turn = parseMemoryRNG memory


-- Checks if hearts have been played before by checking everywhere a card can be played for hearts (current trick, past 
-- tricks, and memory) 
checkBroken :: [(Card, PlayerId)] -> [(Card, PlayerId)] -> String -> Bool 
checkBroken field past memory = hearts field || hearts past  || parseMemoryBroken memory where
        hearts = any (checkSuit Heart . fst )

-- If the input card wins AND this winning card is in the pointCard list
checkLostPoint :: Card -> [(Card, PlayerId)] -> Bool
checkLostPoint card playList = maxOfCardList ((card, "TEST") : playList) == card && card `elem` pointCards

 -- Take the memory string and update it to what we want
constructMemory :: String  -> [(Card, PlayerId)] -> [(Card, PlayerId)] -> Card -> String


-- Construct the memory of if hearts have been broken, by checking if hearts have been played, but also if the next card 
-- played will break hearts
constructMemory memory currentPlay previousPlay toPlay = 
    "RNG" ++ show (1 + parseMemoryRNG memory) ++ "|" -- Update the RNG coefficient.  Always delineate with "RNG" and "|"
 ++ "NUMP" ++ (if null previousPlay 
               then show (parseMemoryNumPlayers memory) -- Use the default saved in memory if by default
               else show (length previousPlay)) ++ "|" -- Get the number of players from the length of the last state
 ++ "BRO" ++ show (checkBroken currentPlay previousPlay memory) ++ "|" -- Use the above function to check broken hearts
 ++ "MPS" ++ concatMap show (map fst previousPlay ++ parseMemoryCards memory) ++ "|" -- Store the cards played through
                -- String manipulation and the function to read cards
 ++ "LOST" ++ show (checkLostPoint toPlay currentPlay || parseMemoryLostRound memory) ++ "|" -- Save if we "lost",
    -- i.e. won a point card


-- the parse functions takes inspiration from the below site (Stack Overflow, of course) in recognising substrings:
-- https://stackoverflow.com/questions/48492142/replace-a-substring-from-a-string-in-haskell
-- The above link mentions how a cons list can be separated into multiple characters and a "rest"

-- Go through a string formatted to store the RNG number, find it, and parse it to an INT
parseMemoryRNG :: String -> Int
parseMemoryRNG ('R' : 'N' : 'G' : rest) = read (parseUntilBreak "" rest) :: Int -- If RNG is found, get the number there
parseMemoryRNG (_:rest) = parseMemoryRNG rest -- keep going through the string
parseMemoryRNG "" = 1 -- return 1 as a default base case

-- Get the number of players from memory.  By default here, it is 1
parseMemoryNumPlayers :: String -> Int
parseMemoryNumPlayers "" = 2
parseMemoryNumPlayers ('N' : 'U' : 'M' : 'P' : rest) = read (parseUntilBreak "" rest) :: Int -- If the stored value is 
    -- found, then return
parseMemoryNumPlayers (_ : rest) = parseMemoryNumPlayers rest

-- Find "BRO" (Broken Hearts) in the memory string and return what it represents (up to "|").  Default to false
parseMemoryBroken :: String -> Bool
parseMemoryBroken "" = False 
parseMemoryBroken ('B' : 'R' : 'O' : rest) = parseUntilBreak "" rest == "True" 
parseMemoryBroken (_ : rest) = parseMemoryBroken rest 

-- Find "LOST" in the memory string and return what it represents.  Keeps track of whether we have any points
parseMemoryLostRound :: String -> Bool
parseMemoryLostRound "" = False 
parseMemoryLostRound ('L' : 'O' : 'S' : 'T' : rest) = parseUntilBreak "" rest == "True" 
parseMemoryLostRound (_ : rest) = parseMemoryLostRound rest 

-- Parses the cards that have been previously played from within memory, to enable smarter decisions to be made
parseMemoryCards :: String -> [Card]
parseMemoryCards "" = []
parseMemoryCards ('M' : 'P' : 'S' : rest) =  parseCardUntilBreak rest []
parseMemoryCards (_ : rest) = parseMemoryCards rest 


-- Parse through a string until the '|' character is hit, converting the string into cards along the way
parseCardUntilBreak :: String -> [Card] -> [Card]
parseCardUntilBreak "" _ = []
parseCardUntilBreak  ('|' : _)  current = current
parseCardUntilBreak (suit : '1' : '0' : rest) current = 
    parseCardUntilBreak rest (( read [suit,'1', '0'] :: Card) : current) 
    -- The suit : 1 : 0 is a special case, as it is the only instance of a card code using three characters
parseCardUntilBreak (suit : number : rest) current = parseCardUntilBreak rest (( read [suit,number] :: Card) : current)
parseCardUntilBreak (_ : _) _ = [] -- There is an error if only one can be matched, so return blank
-- The [suit,number] is necessary to convert from a single char into a string with one character (as a string is just 
    -- [char])

-- Parse through a sting until the '|' character is hit, and return the string (in correct order) when found.
parseUntilBreak :: String -> String ->  String -- goes through a string until a "|" is hit.  
parseUntilBreak _ "" = ""
parseUntilBreak current ('|' : _) = reverse current -- Will be backwards due to recursion, so put it back in order
parseUntilBreak current (top:rest) = parseUntilBreak (top : current) rest

-- Check if the string representation of a card matches one of the cards in a given list.
checkCard :: String -> [Card] -> Bool
checkCard _ [] = False
checkCard cardName deck = any (\currentCard -> show currentCard == cardName) deck -- If the searched card is not
-- present, then the filtered list is going to be empty.  This just checks if the written name of the card matched that 

-- Check if a given Card matches a given suit
checkSuit :: Suit -> Card -> Bool --Returns true if a card of that suit is in the given card
checkSuit suit (Card thisSuit _ ) = suit == thisSuit

-- Simply unpacks the suit of the card
getSuit :: Card -> Suit 
getSuit (Card suit _ ) = suit

-- Simply unpacks the rank of the card
getRank :: Card -> Rank
getRank (Card _ rank ) = rank

-- Loop through the cards and take the highest value.  The two of spades is chosen as default as if there is at least
-- one card in the hand, the ">=" will override it.
highestBrokenLegal :: [Card] -> Card
highestBrokenLegal hand = foldl (\acc element -> 
                                if getRank element >= getRank acc then element else acc ) (Card Spade Two) checkedHand  
    where 
        -- Filter the point cards out UNLESS hearts have been broken
        checkedHand 
            | any (\card -> getSuit card == Heart) hand = filter (\card -> getSuit card == Heart) hand -- If we have any
                    -- hearts, then use them
            | otherwise = hand  -- Otherwise, max card is played

-- Loop through the cards and take the heighest value.  The two of spades is chosen as default as if there is at least
-- one card in the hand, the ">=" will override it.
highestLegal :: [Card] -> [(Card, PlayerId)] -> [(Card, PlayerId)] -> String -> Card
highestLegal hand currentPool pastPool memory = 
        foldl (\acc element -> if getRank element >= getRank acc then element else acc ) (Card Spade Two) checkedHand  
    where 
        checkedHand = 
            if checkBroken currentPool pastPool memory then hand else filter (\card -> getSuit card /= Heart) hand

-- Loop through the cards and take the lowest value (not a heart unless it has to be) for the first round
-- The ace of spades is chosen as default as if there is at least one card in the hand, the "<=" will override it.
lowestFirstLegal :: [Card] -> Card
lowestFirstLegal hand = foldl (\acc element -> 
                                if getRank element <= getRank acc then element else acc ) (Card Heart Ace) checkedHand  
    where 
        checkedHand 
            | any (\card -> getSuit card /= Heart) hand = filter (\card -> getSuit card /= Heart) hand -- Don't use 
                -- hearts if possible
            | otherwise = hand  -- Otherwise, max card is played


-- Get a list of all the cards in the opponent's hand (intended for two players)
getTwoPlayerCards :: [Card] -> [(Card, PlayerId)] -> [(Card, PlayerId)] -> String -> [Card]
getTwoPlayerCards myHand thisTrick lastTrick memory = 
                    filter (\card -> foldl (&&) True (map (notElem card) [myHand, trickCards, lastCards, memCards])  ) 
                        (Card <$> [Spade ..] <*> [Two ..])
                    -- The above lines may be a little convoluted, so to step through them: for each card, map notElem
                    -- over each potential place a card could be (other than the player's hand).  Using foldl, reduce
                    -- each boolean expression in the list made from map into either true or false (only true if the 
                    -- entire list is true).  Each card that passes this test MUST be in the opponent's hand
    where trickCards = map fst thisTrick
          lastCards = map fst lastTrick
          memCards = parseMemoryCards memory

-- Loop through the cards and take the lowest value.  The ace of spades is chosen as default as if there is at least
-- one card in the hand, the "<=" will override it.
lowestLegal :: [Card] -> [(Card, PlayerId)] -> [(Card, PlayerId)] -> String -> Card
lowestLegal hand currentPool pastPool memory = foldl (\acc element -> 
                                if getRank element <= getRank acc then element else acc ) (Card Spade Ace) checkedHand  
   where 
       checkedHand 
        | checkBroken currentPool pastPool memory = hand 
        | not (any (\card -> getSuit card /= Heart) hand) = hand -- break some hearts
        | otherwise = filter (\card -> getSuit card /= Heart) hand -- Only filter if there would be something left

-- returns the highest card below a given card (in the same suit) ([] if there isn't one) by checking if a filtered list
-- is not null
highestBelow :: Card -> [Card] -> [Card]
highestBelow comparable hand = [maximum filteredHand | not (null filteredHand)] where
    filteredHand = filter (\card -> compareSuit card comparable && rankLower card comparable ) hand

-- returns the lowest card below a given card (in the same suit) ([] if there isn't one), in the same way as above.
lowestAbove :: Card -> [Card] -> [Card]
lowestAbove comparable hand = [minimum filteredHand | not (null filteredHand)] where
    filteredHand = filter (\card -> compareSuit card comparable && (getRank card >= getRank comparable) ) hand

-- Input two cards, and returns whether or not their suit is equal by getting their suit using getSuit
compareSuit :: Card -> Card -> Bool
compareSuit card1 card2 = getSuit card1 == getSuit card2

-- returns whether card1 has a lower rank than card2, by comparing their direct rank value (using getRank)
rankLower :: Card -> Card -> Bool
rankLower card1 card2 = getRank card1 < getRank card2


-- returns the maximum card in a list. It starts at the last element, as this is going to be the leading suit 
-- If the current maximum (the accumulator) is larger than the next element, it stays the max
-- If the new card doesn't match the suit of the current max, the old max is preserved
-- As such, the maximum will always be of the correct suit
-- Otherwise, update the maximum!
maxOfCardList :: [(Card, PlayerId)] -> Card
maxOfCardList inputList = foldl (\currentMax nextCard -> 
    if (getRank currentMax > getRank nextCard) || (getSuit currentMax /= getSuit nextCard) 
    then
        currentMax 
    else 
        nextCard
    )
    (fst (last inputList)) -- The initial value is the leading card
    (map fst inputList) -- Mapping over the cards, we don't care about the players right now

-- Counts the number of cards in a suit by reducing it to a single integer value
suitCount :: Suit -> [Card] -> Int 
suitCount suit = foldl (\acc element -> if getSuit element == suit then acc + 1 else acc) 0

-- goes through the cards and picks the suit with the least cards (could be more efficient, however this is functional)
fewestSuit :: [Card] -> Suit
fewestSuit cards = foldl (\acc card -> 
                                    if suitCount (getSuit card) cards < suitCount acc cards then getSuit card else acc) 
    (getSuit (head cards)) cards-- Default is the first card's suit. cards is the card array to loop

-- An easy reference to all point cards, generated using the code from Deck.hs
pointCards :: [Card]
pointCards = Card Spade Queen : (Card  <$> [Heart] <*> [Two ..])

-- This function takes in a list of suits, cards in the hand, and the current and past trick, AND memory.
-- Most of these inputs are to use to call the mehod to check if hearts are broken, however this method effectively
-- attempts to return a card that matches the suit constraints given.  If it fails, it reduces the number of constraints
-- by one and calls itself again.  If there are no cards to satisfy any limitations, just play the lowest valid card.
tryAvoid :: [Suit] -> [Card] -> [(Card, String)] -> [(Card, String)] -> String -> Card
tryAvoid (firstAv : restAv) myCards currentTrick pastTrick memory 
    | checkBroken currentTrick pastTrick memory = 
        if not (any (\card -> getSuit card `notElem` (firstAv : restAv)) myCards)
        then tryAvoid restAv myCards currentTrick pastTrick memory 
        else maximum (filter (\card ->  getSuit card `notElem` (firstAv : restAv)) myCards)
    | otherwise = 
        if not (any (\card ->  getSuit card `notElem` (Heart : firstAv : restAv)) myCards)
        then tryAvoid restAv myCards currentTrick pastTrick memory 
        else maximum (filter (\card ->  getSuit card `notElem` (Heart : firstAv : restAv)) myCards)
tryAvoid [] myCards _ _ _ -- Can't satisfy any limitation
    | any ((/=) Heart . getSuit) myCards = minimum (filter ((/=) Heart . getSuit) myCards)
    | otherwise = minimum myCards  

-- A specialised function to deal with what happens when the opponent has the Queen of Spades, and our player is leading
-- It mainly uses filter, and several other functions defined above, in order to pick out cards that match the 
-- restrictions given.
leadOppWithQueen :: [Card] -> [(Card, String)] -> String -> Suit -> Suit -> Card
leadOppWithQueen myCards prevCards memory least secondLeast
    | suitCount Spade (filter (\card -> card /= Card Spade King && card /= Card Spade Ace) myCards) 
     > suitCount Spade opponentCards = 
        -- Play the highest spade that ISN'T higher than Queen, to draw their queen out
        maximum (filter (\card -> getSuit card == Spade && getRank card /= King && getRank card /= Ace) myCards)
    -- If we have less spades, we must preserve them
    -- only spades left, then we have to play
    | least == Spade && secondLeast == Spade = 
        if not (null (highestBelow (Card Spade Queen) (filter ((==) Spade . getSuit) myCards)))
        then head (highestBelow (Card Spade Queen) (filter ((==) Spade . getSuit) myCards))
        else maximum (filter ((==) Spade . getSuit) myCards) -- May as well play the highest card in case the opponent 
        -- messes up
    | least == Spade = 
        if not (any ((==) secondLeast . getSuit) opponentCards) -- This segment attempts to pick out a valid card given
        then tryAvoid [secondLeast, least] myCards [] prevCards memory -- the constraints that have been applied by the
        else if secondLeast == Heart -- caller of the method, and those given by the game. (Hearts must be broken, etc.)
        then minimum (filter ((==) secondLeast . getSuit) myCards)
        else maximum (filter ((==) secondLeast . getSuit) myCards)
    where opponentCards = getTwoPlayerCards myCards [] prevCards memory -- Get their cardsS
leadOppWithQueen myCards _ _ _ _ = minimum myCards -- This case should NEVER be reached, so if it does, try and deal

-- This function checks several boolean statements to see if the player is in a specific, ideal position to play the 
-- Queen of Spades
checkDangerSpades :: [Card] -> Bool
checkDangerSpades cards 
    -- Basically, check if the cards given contain only Spades higher than Queen.  If so, play it down.
    | suitCount Spade cards == 2 && (Card Spade King `elem` cards) && (Card Spade Ace `elem` cards) = True
    | suitCount Spade cards == 1 && (Card Spade King `elem` cards) = True
    | suitCount Spade cards == 1 && (Card Spade Ace `elem` cards) = True
    | otherwise = False


-- This gets the maximum card in the deck that isn't the Queen of Spades. (Put into a function to avoid loads of nested
-- if statements, etc.)
maximumNotQueen :: [Card] -> Card
maximumNotQueen cards = if length cards == 1 then head cards else maximum (filter (Card Spade Queen /= )cards)


-- The main function to describe all rules.  Mentioned more specifically above in the head of the file
-- Using the functions defined above, and the stategies defined even further above, it attempts to play a turn in such 
-- a way that it will achieve a globally optimal result, even though it may lose the trick.
playCard :: PlayFunc
playCard _ myCards [] Nothing  -- While it is impossible to not have the Two of Clubs here, the error checking makes me
                               -- feel a lot safer
    | checkCard "C2" myCards = (Card Club Two, defaultMemory) 
    | otherwise = (lowestFirstLegal myCards,  defaultMemory) 

    where 
        defaultMemory = "RNG1|NUMP1|BROFalse|MPS|LOSTFalse|"
-- If the first trick (no memory), just DON'T play points.  Also get rid of the highest spade if you can
playCard _ myCards playList Nothing 
    | any  (compareSuit (fst (last playList)))  myCards = 
        (maximum (filter ((==) (getSuit (fst (last playList))) . getSuit) myCards), defaultMemory) -- Play the max card
        -- to lose a big spade earlier


    | otherwise = if (show (lowestFirstLegal myCards) == "SQ") && (length myCards == 1) 
        then (Card Spade Queen ,defaultMemory) -- Only play the queen of spades now if it's the only option
            -- because this is not the ideal situation for the spade queen
        else (lowestFirstLegal (filter (\card -> card /= Card Spade Queen) myCards), defaultMemory)
            -- Otherwise just play the lowest legal card
            where 
                defaultMemory = "RNG1|NUMP1|BROFalse|MPS|LOSTFalse|"
playCard _ myCards [] (Just (states, memory))
    | length states == 4 = -- Different strategy for 4 players

        if checkEarly memory myCards
        then 
            (highestLegal myCards [] states memory, constructMemory memory [] states 
                                                                                (highestLegal myCards [] states memory))
        else
            (lowestLegal myCards [] states memory, constructMemory memory [] states 
                                                                                (lowestLegal myCards [] states memory)) 
    | otherwise = 
        if checkBroken [] states memory && suitCount Heart myCards > 0  && not (parseMemoryLostRound memory)
        then 
            (maximum (filter ((==) Heart . getSuit ) myCards), 
            constructMemory memory [] states (maximum (filter ((==) Heart . getSuit ) myCards))) 
        else if checkCard "SQ" myCards && checkDangerSpades opponentCards 
        then (Card Spade Queen, constructMemory memory [] states (Card Spade Queen)) -- Quick check to see if we can 
            --  trap the opponent with the queen

        else if checkCard "SQ" opponentCards -- We employ a separate strategy if we have the queen
        then
             (leadOppWithQueen myCards states memory least secondLeast, constructMemory memory [] states 
                                                            (leadOppWithQueen myCards states memory least secondLeast))

            -- (lowestLegal myCards [] states memory, constructMemory memory [] states) 
        else if least /= Spade -- This only works without spades, otherwise we just end up playing the queen of spades
        then
            (maximum (filter ((==) least . getSuit) myCards), constructMemory memory [] states
                     (maximum (filter ((==) least . getSuit) myCards)))  -- drop a card from our suit with fewest cards
        else if suitCount Spade myCards - 1 > suitCount Spade opponentCards -- If we have more spades than the opponent
                                                                            -- (not including the queen)           
        then 
            -- Else play the highest non-queen -- Play the highest to stay in control
            (maximum (filter (\card -> getSuit card == Spade && card /= Card Spade Queen) myCards), 
                constructMemory memory [] states (maximum (filter (\card ->
                                                         getSuit card == Spade && card /= Card Spade Queen) myCards))) 
            
        else 
        -- play the lowest legal card
            (lowestLegal myCards [] states memory, constructMemory memory [] states 
                                                                                (lowestLegal myCards [] states memory)) 
            

    where 
          opponentCards = getTwoPlayerCards myCards [] states memory
          least
            | checkBroken [] states memory || not (any ((/=) Heart. getSuit) myCards) = fewestSuit myCards 
            | otherwise = fewestSuit (filter ((/=) Heart . getSuit) myCards) -- get the suit with the least cards (but 
                                                                             -- only hearts if they're broken)
          secondLeast 
            | not (any ((/=) least . getSuit) myCards) = 
                least 
            | checkBroken [] states memory || not (any ((/=) least . getSuit) (filter ((/=) Heart . getSuit) myCards)) = 
                --fewestSuit (filter ((/=) least . getSuit) myCards)
                least
             
            | otherwise = fewestSuit (filter ((/=) least . getSuit) (filter ((/=) Heart . getSuit) myCards)) -- get the 
                                                        -- suit with the least cards (but only hearts if they're broken)
           
-- Play the lowest card in the leading suit, else play the lowest card.  Also set the default memory values
playCard _ myCards playList (Just (states, memory))
    -- To attempt to stop the opponent shooting the moon, get a point if we haven't.
    | getSuit (fst (last playList)) == Heart && suitCount Heart myCards > 0 && parseMemoryLostRound memory = 
        (maximum (filter ((==) Heart . getSuit ) myCards), 
            constructMemory memory playList states (maximum (filter ((==) Heart . getSuit ) myCards))) 
    
    -- Play the same suit as the leading card if we can
    | any  (compareSuit (fst (last playList)))  myCards = 
        if (length playList == length states - 1 || length states /= 4) && (notElem (fst (last playList)) pointCards
         || null (highestBelow (maxOfCardList playList) myCards )) 
            -- If two player AND not a point card in play, OR two player and you'll lose anyway, max card
            -- If you're last in the trick and will lose anyway, or have no point cards, play max
            --getSuit (fst (last playList)) /= Heart && fst (last playList) /= Card Spade Queen
            then
                (maximumNotQueen (filter (compareSuit (fst ( last playList))) myCards), 
                constructMemory memory playList states (maximumNotQueen 
                                                                (filter (compareSuit (fst ( last playList))) myCards)))

        else if not (null (highestBelow (maxOfCardList playList) myCards )) 
        then 
            (head (highestBelow (maxOfCardList playList) (filter (compareSuit (fst ( last playList))) myCards)),
            constructMemory memory playList states (head (highestBelow (maxOfCardList playList) 
                (filter (compareSuit (fst ( last playList))) myCards)))) -- If you have any card in the suit, play it
        
        else if length playList == length states - 1
        then -- This is the last player, so if we lose, offload a big card
            (maximum (filter (compareSuit (fst ( last playList))) myCards), 
            constructMemory memory playList states (maximum (filter (compareSuit (fst ( last playList))) myCards)))
             -- Play the heighest card you have
        else 
            (head (lowestAbove (maxOfCardList playList) (filter (compareSuit (fst ( last playList))) myCards)),
            constructMemory memory playList states (head (lowestAbove (maxOfCardList playList) 
                                                            (filter (compareSuit (fst ( last playList))) myCards))))
    | checkCard "SQ" myCards = (Card Spade Queen , constructMemory memory playList states (Card Spade Queen )) 
                -- Offload the queen as soon as possible
    | otherwise = (highestBrokenLegal myCards, constructMemory memory playList states (highestBrokenLegal myCards))
             -- Otherwise play the highest legal card, to get rid the big cards


-- | Not used, do not remove.
makeBid :: BidFunc
makeBid = undefined

