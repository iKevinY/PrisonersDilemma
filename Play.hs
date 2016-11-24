-- CPSC 312 - 2016 - Games in Haskell
module Play where

-- To run it, try:
-- ghci
-- :load Play
-- start_tournament always_cooperate
-- start_tournament tit_for_tat

import PrisonersDilemma
import System.IO

type TournamentState = (Int,Int,Int)   -- wins, losses, ties


start_tournament ai = play pd (pd Start) ai (0, 0, 0)

play :: Game -> Result -> Player -> TournamentState -> IO TournamentState

play game start opponent tournament_state =
  let (wins, losses,ties) = tournament_state in
   do
      putStrLn ("Tournament results: "++ show wins++ " wins "++show losses++" losses "++show ties++" ties")
      putStrLn "Action? 0=play, 1=exit."
      line <- getLine
      if (read line :: Int)==0
      then
            person_play game start opponent tournament_state
      else
             return tournament_state

person_play :: Game -> Result -> Player -> TournamentState -> IO TournamentState
-- opponent has played, the person must now play
person_play game (EndOfGame 1) opponent (wins,losses,ties) =
   do
      putStrLn "Computer won!"
      play game (game Start) opponent (wins,losses+1,ties)
person_play game (EndOfGame 0) opponent (wins,losses,ties) =
   do
      putStrLn "It's a draw."
      play game (game Start) opponent (wins,losses,ties+1)
person_play game (EndOfGame (-1)) opponent (wins,losses,ties) =
   do
      putStrLn "You won!"
      play game (game Start) opponent (wins+1,losses,ties)
person_play game (ContinueGame state) opponent tournament_state =
   do
      putStrLn ("Move history is "++show (zip (fst state) (snd state))++" (recent -> oldest). 1=coop; 0=defect.")
      line <- getLine
      if (read line :: Int) == 1
        then
          computer_play game (game (Move C state)) opponent tournament_state
        else
          computer_play game (game (Move D state)) opponent tournament_state


computer_play :: Game -> Result -> Player -> TournamentState -> IO TournamentState
-- computer_play game current_result opponent tournament_state
-- person has played, the computer must now play
computer_play game (EndOfGame 1) opponent (wins,losses,ties) =
   do
      putStrLn "You won!"
      play game (game Start) opponent (wins+1,losses,ties)
computer_play game (EndOfGame 0) opponent (wins,losses,ties) =
   do
      putStrLn "It's a draw."
      play game (game Start) opponent (wins,losses,ties+1)

computer_play game result opponent tournament_state =
      let ContinueGame state = result
          opponent_move = opponent game result
        in
          do
            person_play game (game (Move opponent_move state)) opponent tournament_state

