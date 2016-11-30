module Play where

import PrisonersDilemma
import System.IO

-- Use `play <ai_strategy>` to start a game
play ai_strategy = person_play pd (pd Start) ai_strategy

-- opponent has played, the person must now play
person_play game (EndOfGame 1 score) ai_strategy =
  do putStrLn "------------ *Game Over* --------------"
     putStrLn "Results:"
     putStrLn("-> Final Score:  " ++ (show score) ++ " (highest possible group score: " ++ show (3*totalRounds,3*totalRounds) ++ ")")
     putStrLn("-> Your score:     " ++ (show (snd score)) ++ "    (highest possible individual score: " ++ show (4*totalRounds) ++ ")")
     putStrLn("-> Agent's score:  " ++ (show (fst score)) ++ "    (highest possible individual score: " ++ show (4*totalRounds) ++ ")")

person_play game (EndOfGame 0 score) ai_strategy =
  do putStrLn "------------ *Game Over* --------------"
     putStrLn "Results:"
     putStrLn("-> Final Score:  " ++ (show score) ++ " (highest possible group score: " ++ show (3*totalRounds,3*totalRounds) ++ ")")
     putStrLn("-> Your score:     " ++ (show (snd score)) ++ "    (highest possible individual score: " ++ show (4*totalRounds) ++ ")")
     putStrLn("-> Agent's score:  " ++ (show (fst score)) ++ "    (highest possible individual score: " ++ show (4*totalRounds) ++ ")")

person_play game (EndOfGame (-1) score) ai_strategy =
  do putStrLn "------------ *Game Over* --------------"
     putStrLn "Results:"
     putStrLn("-> Final Score:  " ++ (show score) ++ " (highest possible group score: " ++ show (3*totalRounds,3*totalRounds) ++ ")")
     putStrLn("-> Your score:     " ++ (show (snd score)) ++ "    (highest possible individual score: " ++ show (4*totalRounds) ++ ")")
     putStrLn("-> Agent's score:  " ++ (show (fst score)) ++ "    (highest possible individual score: " ++ show (4*totalRounds) ++ ")")

person_play game (ContinueGame state) ai_strategy =
  do
    putStrLn("------------- *Round " ++ show (fromIntegral(length (fst state) + 1))++ "* --------------")
    putStrLn("-> Your moves (recent -> late): " ++ show (fst state))
    putStrLn("-> the agent's moves:           " ++ show (snd state))
    putStrLn("Pick a move... (1 = cooperate, 0 = defect)")
    line <- getLine
    if (read line :: Int) == 1
    then
      computer_play game (game (Move C state)) ai_strategy
      else if (read line::Int) == 0 
      then computer_play game (game (Move D state)) ai_strategy
        else do putStrLn "###########################################"
                putStrLn "#                                         #"
                putStrLn "#    You had a typo. Redo this round:     #"
                putStrLn "#                                         #"
                putStrLn "###########################################"
                person_play game (ContinueGame state) ai_strategy


-- person has played, the computer must now play
computer_play game (ContinueGame state) ai_strategy =
  person_play game (game (Move (ai_strategy state) state)) ai_strategy
