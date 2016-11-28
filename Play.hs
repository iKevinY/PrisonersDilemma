-- CPSC 312 - 2016 - Games in Haskell
module Play where

-- To run it, try:
-- ghci
-- :load Play
-- play always_cooperate
-- play tit_for_tat

import PrisonersDilemma
import System.IO


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
    putStrLn ("Move history is "++show (zip (fst state) (snd state))++" (recent -> oldest). 1=coop; 0=defect.")
    line <- getLine
    if (read line :: Int) == 1
    then
      computer_play game (game (Move C state)) opponent
    else
      computer_play game (game (Move D state)) opponent


-- computer_play game current_result opponent
-- person has played, the computer must now play
computer_play game result opponent =
  let ContinueGame state = result
    opponent_move = opponent game result
  in
    do
      person_play game (game (Move opponent_move state)) opponent
