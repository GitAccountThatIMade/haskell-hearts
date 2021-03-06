import Control.Monad
import System.Exit

import EitherIO
import Hearts.Types
import Hearts.Play(playGame)
import Game(newPlayer)
import Logs(writeGame)
import Naive

import safe qualified Player


-- | Test a game of the Player against itself.
test_one_play :: [Player] -> IO Bool
test_one_play players = do
  played <- runEitherIO $ playGame 10000 players
  case played of
    Right gr -> writeGame gr >> return True
    -- Beware, you need to capture the error if run headless
    Left e -> putStrLn "" >> print e >> return False

main :: IO ()
main = do
  played <- test_one_play $ map player ["P1", "P2", "P3", "P4"]
  if played
     then exitSuccess
     else exitFailure
  where
    player name = newPlayer name Player.playCard Player.makeBid
  -- played <- test_one_play [newPlayer "HerzaQuinn" Player.playCard Player.makeBid, newPlayer "SmolValid" Naive.playCard Naive.makeBid]
  -- if played
  --    then exitSuccess
  --    else exitFailure

