module Checkers (checkers) where
import Games
import RectBoard

winnerCheckers :: RectBoard -> [PlayerId] -> Maybe PlayerId
winnerCheckers board players =
  case [player
       | player <- players,
         null $ playerPieces board (1 - player)] of
       [p] -> Just p
       _ -> Nothing

isDrawCheckers :: RectBoard -> [PlayerId] -> Bool
isDrawCheckers board players =
  all (\p -> null $ moves p board) players
  
checkers :: Game RectBoard RectBoardMove
checkers =
  initGame
  (initBoard (8, 8) $
   [(Checker 0 [CheckerKing 0] S, (0, i)) | i <- [0, 2, 4, 6]] ++
   [(Checker 0 [CheckerKing 0] S, (1, i)) | i <- [1, 3, 5, 7]] ++
   [(Checker 0 [CheckerKing 0] S, (2, i)) | i <- [0, 2, 4, 6]] ++
   [(Checker 1 [CheckerKing 1] N, (5, i)) | i <- [1, 3, 5, 7]] ++
   [(Checker 1 [CheckerKing 1] N, (6, i)) | i <- [0, 2, 4, 6]] ++
   [(Checker 1 [CheckerKing 1] N, (7, i)) | i <- [1, 3, 5, 7]])
  2
  winnerCheckers
  isDrawCheckers
  (\board player -> length (moves player board) > 0)
  
