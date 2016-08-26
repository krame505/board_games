module Checkers (checkers) where
import Games
import RectBoard

data CheckersPiece = Checker PlayerId Direction
                   | CheckerKing PlayerId
                   deriving (Show, Eq)

instance Piece_ CheckersPiece where
  pieceMoves board (x, y) (Checker _ N) =
    filter (isOpenMove board) [DirectMove x y (x - 1) (y - 1),
                               DirectMove x y (x - 1) (y + 1)]
  pieceMoves board (x, y) (Checker _ S) =
    filter (isOpenMove board) [DirectMove x y (x + 1) (y + 1),
                               DirectMove x y (x + 1) (y - 1)]
  pieceMoves board (x, y) (Checker _ E) =
    filter (isOpenMove board) [DirectMove x y (x + 1) (y + 1),
                               DirectMove x y (x - 1) (y + 1)]
  pieceMoves board (x, y) (Checker _ W) =
    filter (isOpenMove board) [DirectMove x y (x + 1) (y - 1),
                               DirectMove x y (x - 1) (y - 1)]
  pieceMoves board (x, y) (CheckerKing _) =
    filter (isOpenMove board) [DirectMove x y (x + 1) (y - 1),
                               DirectMove x y (x + 1) (y + 1),
                               DirectMove x y (x - 1) (y - 1),
                               DirectMove x y (x - 1) (y + 1)]
  
  name (Checker _ _) = "checker"
  name (CheckerKing _) = "checker king"
  
  label (Checker player _) = if player `mod` 2 == 0 then '⛀' else '⛂'
  label (CheckerKing player) = if player `mod` 2 == 0 then '⛁' else '⛃'
  
  owner (Checker player _) = player
  owner (CheckerKing player) = player

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
   [(Piece $ Checker 0 S, (0, i)) | i <- [0, 2, 4, 6]] ++
   [(Piece $ Checker 0 S, (1, i)) | i <- [1, 3, 5, 7]] ++
   [(Piece $ Checker 0 S, (2, i)) | i <- [0, 2, 4, 6]] ++
   [(Piece $ Checker 1 N, (5, i)) | i <- [1, 3, 5, 7]] ++
   [(Piece $ Checker 1 N, (6, i)) | i <- [0, 2, 4, 6]] ++
   [(Piece $ Checker 1 N, (7, i)) | i <- [1, 3, 5, 7]])
  2
  winnerCheckers
  isDrawCheckers
  (\board player -> length (moves player board) > 0)
  
