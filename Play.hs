module Play where

import PrisonersDilemma
import System.IO

-- Use `play <opponent-type>` to start a game
play opponent_type = person_play pd (pd Start) opponent_type

-- opponent has played, the person must now play
person_play game (EndOfGame 1) opponent =
  do putStrLn "Computer won!"

person_play game (EndOfGame 0) opponent =
  do putStrLn "It's a draw."

person_play game (EndOfGame (-1)) opponent =
  do putStrLn "You won!"

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
