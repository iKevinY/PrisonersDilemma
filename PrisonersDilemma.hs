module PrisonersDilemma where


data CoopOrDefect = C | D deriving (Enum, Show, Eq)
not C = D
not D = C

type AMove = CoopOrDefect          -- a move for a player
type State = ([AMove], [AMove])    -- (my moves, opponent's moves)

data Action = Move AMove State     -- perform a move to a state
            | Start                -- returns starting state

data Result = EndOfGame Int        -- end of game
            | ContinueGame State   -- continue with next game state
                deriving (Eq, Show)

type Game = Action -> Result

type Player = State -> AMove

------ Iterative Prisoner's Dilemma -------

pd :: Game
pd (Move move (mine, others))
    | (length others) == 5 = EndOfGame (winner mine others)
    | otherwise            = ContinueGame (others, move:mine)

pd Start = ContinueGame ([],[])


-- Encode the PD scoring mechanism into function
{-
     A \ B   Coop    Defect
    Coop     3\3       1\4
    Defect   4\1       2\2
-}

score C C = (3, 3)
score C D = (1, 4)
score D C = (4, 1)
score D D = (2, 2)


-- Element-wise summation of a tuple
sumtuple x y = ((fst x) + (fst y), (snd x) + (snd y))


-- Returns 1 if the first player won, 0 if draw, -1 if second player won
winner amoves bmoves
    | ((fst gamescore) > (snd gamescore)) = 1
    | ((fst gamescore) < (snd gamescore)) = -1
    | otherwise = 0
    where
        gamescore = foldr (\(x, y) acc -> sumtuple (score x y) acc) (0, 0) (zip amoves bmoves)


------- AI Strategies -------

always_cooperate :: Player
always_cooperate (_, _) = C

always_defect :: Player
always_defect (_, _) = D

alternating :: Player
alternating ([], _) = C
alternating (yours, _) = PrisonersDilemma.not (head yours)

tit_for_tat :: Player
tit_for_tat ([], _) = C
tit_for_tat (_, []) = C

-- Don't cheat by looking ahead at a move you shouldn't see
tit_for_tat (yours, others)
    | (length others) > (length yours)  = others !! 1
    | otherwise                         = head others
