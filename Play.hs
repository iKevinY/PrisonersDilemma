module Play where

import PrisonersDilemma
import System.IO

-- Use `play <ai_strategy>` to start a game
play ai_strategy = person_play pd (pd Start) ai_strategy

-- Game is over
person_play game (EndOfGame score) ai_strategy =
  do putStrLn "------------ Game Over --------------"
     putStrLn("Final score: " ++ (show (fst score)) ++ "-" ++ (show (snd score)))
     putStrLn("Best possible score: " ++ show (4 * PrisonersDilemma.totalRounds))
     putStrLn("Worst possible score: " ++ show (1 * PrisonersDilemma.totalRounds))

-- Computer has played, so now the person must play.
person_play game (ContinueGame state) ai_strategy =
  do
    putStrLn("------------ Round " ++ (show (length (fst state) + 1)) ++ " --------------")
    putStrLn("Your moves (new -> old): " ++ show (fst state))
    putStrLn("Their moves:             " ++ show (snd state))
    putStrLn("Pick a move... (1 = cooperate, 0 = defect)")
    line <- getLine
    if (read line :: Int) == 1
    then
      computer_play game (game (Move C state)) ai_strategy
      else if (read line :: Int) == 0
      then computer_play game (game (Move D state)) ai_strategy
        else do putStrLn "You had a typo. Type again."
                person_play game (ContinueGame state) ai_strategy


-- person has played, the computer must now play
computer_play game (ContinueGame state) ai_strategy =
  person_play game (game (Move (ai_strategy state) state)) ai_strategy
