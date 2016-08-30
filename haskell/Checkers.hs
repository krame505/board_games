{-# LANGUAGE MultiParamTypeClasses #-}
import Games
import RectBoard

import RandomPlayer
import HumanPlayer
import MCTSPlayer

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

playerOptions :: [(String, Player RectBoard RectBoardMove)]
playerOptions = [("random", randomPlayer),
                 ("human", humanPlayer),
                 ("mcts", mctsPlayer)]
          
main :: IO ()
main = do args <- getArgs
          let players =
                [case lookup arg playerOptions of
                    Just player -> player
                    Nothing -> error $ "Invalid player " ++ arg
                | arg <- args] ++ repeat humanPlayer
          winner <- playGame players True
          return ()
