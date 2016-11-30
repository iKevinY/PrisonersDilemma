module Play where

import PrisonersDilemma
import System.IO

-- Use `play <opponent-type>` to start a game
play opponent_type = person_play pd (pd Start) opponent_type

-- opponent has played, the person must now play (or game is over)
person_play game (EndOfGame score) opponent =
  do putStrLn "------------ Game Over --------------"
     putStrLn("Final score: " ++ (show (fst score)) ++ "-" ++ (show (snd score)))

person_play game (ContinueGame state) opponent =
  do
    putStrLn("Your moves (new -> old): " ++ show (fst state))
    putStrLn("Their moves:             " ++ show (snd state))
    putStrLn("Pick a move... (1 = cooperate, 0 = defect)")
    line <- getLine
    if (read line :: Int) == 1
    then
      computer_play game (game (Move C state)) opponent
      else if (read line::Int) == 0
      then computer_play game (game (Move D state)) opponent
        else do putStrLn "You had a typo. Type again."
                person_play game (ContinueGame state) opponent


-- person has played, the computer must now play
computer_play game (ContinueGame state) opponent =
  person_play game (game (Move (opponent state) state)) opponent
