{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses #-}
module RectBoard where
import Data.List (intercalate)
import Data.Maybe
--import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!), size, insert, toList, fromList)
import Data.Text (justifyLeft)
import Data.Char

import System.Console.ANSI

import Games

-- Board games
newtype RectBoard = RectBoard (Map Int (Map Int (Maybe Piece)))
                  deriving Eq
data RectBoardMove = DirectMove Int Int Int Int
                   | CompositeMove [RectBoardMove]
                  deriving (Show, Eq) -- TODO: better show

type Loc = (Int, Int)

directMove :: Loc -> Loc -> RectBoardMove
directMove (x1, y1) (x2, y2) = DirectMove x1 y1 x2 y2

instance GameState RectBoard RectBoardMove where
  --moves :: RectBoard -> PlayerId -> [RectBoard]
  moves player (RectBoard board) =
    [move
    | (i, row) <- toList board,
      (j, elem) <- toList row,
      piece <- maybeToList elem,
      move <- pieceMoves (RectBoard board) (i, j) piece,
      owner piece == player]

  -- TODO: This assumes columns are less than 10 tall
  readMove player [col1, row1, ' ', col2, row2] board =
    readMove player [col1, row1, col2, row2] board
  readMove player [col1, row1, col2, row2] board | isDigit row1 && isDigit row2 =
    Left $ DirectMove x1 y1 x2 y2
    where
      x1 = height board - read [row1]
      y1 = ord col1 - ord 'a'
      x2 = height board - read [row2]
      y2 = ord col2 - ord 'a'
  readMove player [col1, row1] board =
    if length options == 1
    then Left $ head options
    else Right $ "Invalid or ambiguous move"-- ++ show options
    where
      x2 = height board - read [row1]
      y2 = ord col1 - ord 'a'
      options = [move | move <- moves player board,
                 case move of
                   DirectMove _ _ x y -> x == x2 && y == y2
                   _ -> False]
  readMove player _ board = Right "Invalid move syntax"

  doMove (DirectMove x1 y1 x2 y2) board =
    set (x2, y2) (get (x1, y1) board) $ set (x1, y1) Nothing board
  -- TODO: Correct handling of swap moves
  doMove (CompositeMove moves) board = foldr doMove board moves

instance Show RectBoard where
  show (RectBoard board) =
    "\n  " ++  intercalate "  " [[a] | a <- take (size $ board!0)['a'..]] ++ "\n" ++
    intercalate "\n" [
      show (size board - i) ++ " " ++
      intercalate " " [
        case elem of
          Just piece ->
            setSGRCode [SetColor Foreground Vivid $ color piece,
                        SetColor Background Dull $ if (i + j) `mod` 2 == 1
                                                   then Red
                                                   else Black,
                        SetConsoleIntensity BoldIntensity] ++
            [label piece] ++ " "
          Nothing ->
            setSGRCode [SetColor Foreground Dull Blue,
                        SetColor Background Dull $ if (i + j) `mod` 2 == 1
                                                   then Red
                                                   else Black,
                        SetConsoleIntensity NormalIntensity] ++
           [chr (j + ord 'a')] ++ show (size board - i)
        ++ setSGRCode [Reset]
        | (j, elem) <- toList row] ++ " " ++
      show (size board - i)
      | (i, row) <- toList board] ++
    "\n  " ++  intercalate "  " [[a] | a <- take (size $ board!0)['a'..]] ++ "\n"


-- Board helper functions
get :: Loc -> RectBoard -> Maybe Piece
get (x, y) (RectBoard board) =
  if x > size board || x < 0
  then error $ "x coordinate out of bounds: " ++ show x
  else if y > size (board!0) || y < 0
  then error $ "y coordinate out of bounds: " ++ show y
  else board!x!y

set :: Loc -> Maybe Piece -> RectBoard -> RectBoard
set (x, y) piece (RectBoard board) =
  if x >= size board || x < 0
  then error $ "x coordinate out of bounds: " ++ show x
  else if x >= size (board!0) || x < 0
  then error $ "y coordinate out of bounds: " ++ show y
  else RectBoard $ insert x (insert y piece $ board!x) board

width :: RectBoard -> Int
width (RectBoard board) = size $ board!0

height :: RectBoard -> Int
height (RectBoard board) = size board

pieces :: RectBoard -> [Piece]
pieces (RectBoard board) =
  [piece
  | (i, row) <- toList board,
    (j, elem) <- toList row,
    piece <- maybeToList elem]

playerPieces :: RectBoard -> PlayerId -> [Piece]
playerPieces (RectBoard board) player =
  [piece
  | (i, row) <- toList board,
    (j, elem) <- toList row,
    piece <- maybeToList elem,
    owner piece == player]

empty :: (Int, Int) -> RectBoard
empty (w, h) =
  RectBoard $ fromList $ zip [0..w-1] $ repeat $ fromList $ zip [0..h-1] $ repeat Nothing

initBoard :: (Int, Int) -> [(Piece, Loc)] -> RectBoard
initBoard size pieces =
  foldr (\(piece, loc) board -> set loc (Just piece) board) (empty size) pieces

isValidLoc :: RectBoard -> Loc -> Bool
isValidLoc board (x, y) = x < height board && x > 0 && y < width board && y > 0

isOpen :: RectBoard -> Loc -> Bool
isOpen board loc = isNothing $ get loc board

isOpenMove :: RectBoard -> RectBoardMove -> Bool
isOpenMove board (DirectMove x1 y1 x2 y2) = isValidLoc board (x2, y2) && isOpen board (x2, y2)
-- TODO: Correct handling of swap moves
isOpenMove board (CompositeMove moves) = all (isOpenMove board) moves

-- Pieces
class (Show p, Eq p) => Piece_ p where
  pieceMoves :: RectBoard -> (Int, Int) -> p -> [RectBoardMove]

  name :: p -> String
  label :: p -> Char
  color :: p -> Color
  owner :: p -> PlayerId

  color p = [White, Green, Yellow, Blue, Magenta, Cyan]!!(owner p `div` 2)

data Piece = forall p. Piece_ p => Piece p
--deriving instance Show Piece

instance Show Piece where
  show (Piece p) = show p

instance Eq Piece where
  -- Hacky, but the best we can do since p1 and p2 might not be the same type
  Piece p1 == Piece p2 = show p1 == show p2

instance Piece_ Piece where
  pieceMoves board loc (Piece p) = pieceMoves board loc p
  name (Piece p) = name p
  label (Piece p) = label p
  color (Piece p) = color p
  owner (Piece p) = owner p

-- Pieces declared in game modules
data Direction = N | S | E | W
               deriving (Show, Eq)
