{-# LANGUAGE MultiParamTypeClasses #-}
import Games
import RectBoard

import RandomPlayer
import HumanPlayer
import MCTSPlayer
import MinMaxPlayer
import TrainedPlayer

--import qualified Data.Map.Strict as Map
import Data.List
import Data.Maybe
import Data.Function

import System.Environment

instance Game RectBoard RectBoardMove where
  initial = GameState {
    board=initBoard (8, 8) $
          [(Checker 0 [CheckerKing 0] S, (0, i)) | i <- [0, 2, 4, 6]] ++
          [(Checker 0 [CheckerKing 0] S, (1, i)) | i <- [1, 3, 5, 7]] ++
          [(Checker 0 [CheckerKing 0] S, (2, i)) | i <- [0, 2, 4, 6]] ++
          [(Checker 1 [CheckerKing 1] N, (5, i)) | i <- [1, 3, 5, 7]] ++
          [(Checker 1 [CheckerKing 1] N, (6, i)) | i <- [0, 2, 4, 6]] ++
          [(Checker 1 [CheckerKing 1] N, (7, i)) | i <- [1, 3, 5, 7]],
    players=initPlayers 2}

  -- TODO: Winning cases when no moves possible not exactly right
  winner game =
    case [player | player <- players game,
          (null $ playerPieces (board game) (1 - player)) ||
          (null $ moves (1 - player) $ board game)] of
      [p] -> Just p
      _ -> Nothing

  isDraw game = all (\p -> null $ moves p $ board game) $ players game

  isActive game player = True

  features = rectBoardFeatures [0..1] ["checker", "checker king"]

manualMinMaxPlayer depth = minMaxPlayer score heuristic (depth - 1) depth
  where
    multipliers = [("checker", 1), ("checker king", 4)]
    weights =
      [("checker",
        [[4, 4, 4, 4, 4, 4, 4, 4],
         [4, 3, 3, 3, 3, 3, 3, 4],
         [4, 3, 2, 2, 2, 2, 3, 4],
         [4, 3, 2, 1, 1, 2, 3, 4],
         [4, 3, 2, 1, 1, 2, 3, 4],
         [4, 3, 2, 2, 2, 2, 3, 4],
         [4, 3, 3, 3, 3, 3, 3, 4],
         [4, 4, 4, 4, 4, 4, 4, 4]]),
       ("checker king",
        [[1, 1, 1, 1, 1, 1, 1, 1],
         [1, 1, 1, 1, 1, 1, 1, 1],
         [1, 1, 1, 1, 1, 1, 1, 1],
         [1, 1, 1, 1, 1, 1, 1, 1],
         [1, 1, 1, 1, 1, 1, 1, 1],
         [1, 1, 1, 1, 1, 1, 1, 1],
         [1, 1, 1, 1, 1, 1, 1, 1],
         [1, 1, 1, 1, 1, 1, 1, 1]])]
    score maxPlayer game =
      fromIntegral $ sum [fromJust (lookup (name piece) multipliers) *
                          (fromJust (lookup (name piece) weights)!!i!!j) *
                          if owner piece == maxPlayer then 1 else -1
                         | ((i, j), piece) <- pieces $ board game]
    heuristic maxPlayer = compare `on` score maxPlayer

type CheckersGameState = GameState RectBoard RectBoardMove

playerOptions :: [(String, Player RectBoard RectBoardMove)]
playerOptions = [("random", randomPlayer),
                 ("human", humanPlayer),
                 ("mcts", mctsPlayer 500 12),
                 ("minmax", manualMinMaxPlayer 6)]
          
main :: IO ()
main = do args <- getArgs
          if length args == 1 && head args == "build"
            then buildData "checkers_mcts.txt" (initial :: CheckersGameState)
                 8 1000 10
            else do
            let players =
                  [case lookup arg playerOptions of
                      Just player -> player
                      Nothing -> error $ "Invalid player " ++ arg
                  | arg <- args] ++ repeat humanPlayer
            winner <- playGame players True
            return ()
