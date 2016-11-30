module Play where

import PrisonersDilemma as PD
import System.IO

-- Use `play <ai_strategy>` to start a game
play ai_strategy = person_play pd (pd Start) ai_strategy

-- Game is over
person_play game (EndOfGame final_score) ai_strategy =
  do putStrLn "------------ Game Over --------------"
     putStrLn("Final score (yours, agent's): " ++ show ((fst final_score),(snd final_score)))
     putStrLn("* Best possible score: " ++ show ((fst (PD.score D C)) * PD.totalRounds))
     putStrLn("* Worst possible score: " ++ show ((snd (PD.score D C)) * PD.totalRounds))
     putStrLn("* Best possible group score: " ++ show ((fst (PD.score C C)) * PD.totalRounds, (fst (PD.score C C)) * PD.totalRounds))
     putStrLn("* Worst possible group score: " ++ show ((fst (PD.score D D)) * PD.totalRounds, (fst (PD.score D D)) * PD.totalRounds))

-- Computer has played, so now the person must play.
person_play game (ContinueGame state) ai_strategy =
  do
    putStrLn("------------ Round " ++ (show (length (fst state) + 1)) ++ " --------------")
    putStrLn("Your moves (recent -> early):  " ++ show (fst state))
    putStrLn("The agent's moves:             " ++ show (snd state))
    putStrLn("Pick a move... (1 = cooperate, 0 = defect)")
    line <- getLine
    if (read line :: Int) == 1
    then
      computer_play game (game (Move C state)) ai_strategy
      else if (read line :: Int) == 0
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
