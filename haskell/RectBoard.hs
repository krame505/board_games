{-# LANGUAGE MultiParamTypeClasses #-}
module RectBoard where
import Data.List (intercalate)
import Data.Maybe
--import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!), size, insert, toList, fromList)
import Data.Text (justifyLeft)
import Data.Char
import Data.Typeable

import System.Console.ANSI

import Games

-- Board games
newtype RectBoard = RectBoard (Map Int (Map Int (Maybe Piece)))
                  deriving (Show, Read, Eq, Ord)
data RectBoardMove = DirectMove Loc Loc
                   | SwapMove Loc Loc
                   | CaptureMove Loc PlayerId RectBoardMove
                   | PromoteMove Loc Piece RectBoardMove
                   | SeqMove [RectBoardMove]
                  deriving (Show, Read, Eq, Ord) -- TODO: better show

type Loc = (Int, Int)

instance Board RectBoard RectBoardMove where
  moves player (RectBoard board) =
    [move
    | (i, row) <- toList board,
      (j, elem) <- toList row,
      piece <- maybeToList elem,
      move <- pieceMoves (RectBoard board) (i, j) piece,
      owner piece == player]

  -- TODO: This assumes columns are less than 10 tall
  readMove player (col1:row1:' ':col2:row2:promote) board =
    readMove player (col1:row1:col2:row2:promote) board
  readMove player (col1:row1:col2:row2:' ':promote) board =
    readMove player (col1:row1:col2:row2:promote) board
  readMove player (col1:row1:col2:row2:promote) board | isDigit row1 && isDigit row2 && not (null promote) =
    selectMove player (Just loc1, Just loc2, Just promote) board
    where loc1 = (height board - read [row1], ord col1 - ord 'a')
          loc2 = (height board - read [row2], ord col2 - ord 'a')
  readMove player [col1, row1, col2, row2] board | isDigit row1 && isDigit row2 =
    selectMove player (Just loc1, Just loc2, Nothing) board
    where loc1 = (height board - read [row1], ord col1 - ord 'a')
          loc2 = (height board - read [row2], ord col2 - ord 'a')
  readMove player [col1, row1] board | isDigit row1 =
    selectMove player (Nothing, Just loc, Nothing) board
    where loc = (height board - read [row1], ord col1 - ord 'a')
  readMove player _ board = Right "Invalid move syntax"

  showMove (DirectMove loc1 loc2) board =
    "moved " ++ showBoardLoc board loc1 ++ " to " ++ showLoc board loc2
  showMove (SwapMove loc1 loc2) board =
    "swapped " ++ showBoardLoc board loc1 ++ " and " ++ showBoardLoc board loc2
  showMove (CaptureMove loc _ m) board = 
    showMove m board ++ ", captured " ++ showBoardLoc board loc
  showMove (PromoteMove loc piece m) board =
    showMove m board ++ ", promoted to " ++ name piece
  showMove (SeqMove []) board = ""
  showMove (SeqMove [m]) board = showMove m board
  showMove (SeqMove (m:ms)) board =
    showMove m board ++ ", " ++ showMove (SeqMove ms) (doMove m board)

  doMove (DirectMove loc1 loc2) board =
    set loc2 (get loc1 board) $ set loc1 Nothing board
  doMove (SwapMove loc1 loc2) board =
    set loc2 (get loc1 board) $ set loc1 (get loc2 board) board
  doMove (CaptureMove loc _ m) board = set loc Nothing (doMove m board)
  doMove (PromoteMove loc piece m) board = set loc (Just piece) (doMove m board)
  doMove (SeqMove moves) board = foldl (\b m -> doMove m b) board moves

  display (RectBoard board) =
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

showBoardLoc :: RectBoard -> Loc -> String
showBoardLoc board loc = case get loc board of
  Just piece -> name piece ++ " at " ++ showLoc board loc
  Nothing -> undefined

showLoc :: RectBoard -> Loc -> String
showLoc board (x, y) = (chr $ ord 'a' + y) : (show $ height board - x)

selectMove :: PlayerId -> (Maybe Loc, Maybe Loc, Maybe String) ->
              RectBoard -> Either RectBoardMove String
selectMove player (loc1, loc2, promote) board =
  if null options
  then Right $ "Invalid move"-- ++ show options
  else if length options == 1
  then Left $ head options
  else Right $ "Ambiguous move"-- ++ show options
  where
    options =
      [move | move <- moves player board,
       case loc1 of
         Just loc ->
           isOccupied board loc && not (isOccupied (doMove move board) loc)
         Nothing -> True,
       case loc2 of
         Just loc ->
           not (isOccupied board loc) && isOccupied (doMove move board) loc
         Nothing -> True,
       case (promote, loc2) of
         (Just p, Just loc) ->
           case get loc $ doMove move board of
             Just piece -> name piece == p
             Nothing -> True
         _ -> True]

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
  else if y >= size (board!0) || y < 0
  then error $ "y coordinate out of bounds: " ++ show y
  else RectBoard $ insert x (insert y piece $ board!x) board

width :: RectBoard -> Int
width (RectBoard board) = size $ board!0

height :: RectBoard -> Int
height (RectBoard board) = size board

elems :: RectBoard -> [(Loc, Maybe Piece)]
elems (RectBoard board) =
  [((i, j), elem)
  | (i, row) <- toList board,
    (j, elem) <- toList row]

pieces :: RectBoard -> [(Loc, Piece)]
pieces (RectBoard board) =
  [((i, j), piece)
  | (i, row) <- toList board,
    (j, elem) <- toList row,
    piece <- maybeToList elem]

playerPieces :: RectBoard -> PlayerId -> [(Loc, Piece)]
playerPieces (RectBoard board) player =
  [((i, j), piece)
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
isValidLoc board (x, y) = x < height board && x >= 0 && y < width board && y >= 0

isOccupied :: RectBoard -> Loc -> Bool
isOccupied board loc = isJust $ get loc board

isValidMove :: RectBoard -> RectBoardMove -> Bool
isValidMove board (DirectMove loc1 loc2) =
  isValidLoc board loc1 && isValidLoc board loc2 &&
  isOccupied board loc1 && not (isOccupied board loc2)
isValidMove board (SwapMove loc1 loc2) =
  isValidLoc board loc1 && isValidLoc board loc2 &&
  isOccupied board loc1 && isOccupied board loc2
isValidMove board (CaptureMove loc p m) =
  isValidMove board m && isValidLoc board loc && case get loc (doMove m board) of
    Just piece -> owner piece /= p
    Nothing -> False
isValidMove board (PromoteMove loc _ m) =
  isValidMove board m && isValidLoc board loc && isOccupied board loc
isValidMove board (SeqMove (m:ms)) =
  isValidMove board m && isValidMove (doMove m board) (SeqMove ms)
isValidMove board (SeqMove []) = True

rectBoardFeatures :: [PlayerId] -> [String] -> RectBoard -> [Int]
rectBoardFeatures players pieceNames board =
  -- TODO: Try excluding redundant sums
  [sum [1 | (_, piece) <- playerPieces board player, name piece == pieceName]
  | pieceName <- ["checker", "checker king"],
    player <- [0..1]] ++
  [case elem of
      Just piece -> if name piece == pieceName then 1 else 0
      Nothing -> 0
  | pieceName <- ["checker", "checker king"],
    player <- [0..1],
    (_, elem) <- elems board]

-- Pieces
data Piece = Checker PlayerId [Piece] Direction
           | CheckerKing PlayerId
           deriving (Show, Read, Eq, Ord)
                    
data Direction = N | S | E | W
               deriving (Show, Read, Eq, Ord)

pieceMoves :: RectBoard -> Loc -> Piece -> [RectBoardMove]
pieceMoves board loc (Checker owner promotions N) =
  checkersMoves board loc owner promotions
  (\(x, y) -> x == 0)
  [\(x, y) -> (x - 1, y + 1),
   \(x, y) -> (x - 1, y - 1)]
pieceMoves board loc (Checker owner promotions S) =
  checkersMoves board loc owner promotions
  (\(x, y) -> x == height board - 1)
  [\(x, y) -> (x + 1, y + 1),
   \(x, y) -> (x + 1, y - 1)]
pieceMoves board loc (Checker owner promotions E) =
  checkersMoves board loc owner promotions
  (\(x, y) -> y == 0)
  [\(x, y) -> (x + 1, y + 1),
   \(x, y) -> (x - 1, y + 1)]
pieceMoves board loc (Checker owner promotions W) =
  checkersMoves board loc owner promotions
  (\(x, y) -> y == width board - 1)
  [\(x, y) -> (x + 1, y - 1),
   \(x, y) -> (x - 1, y - 1)]
pieceMoves board loc (CheckerKing owner) =
  checkersMoves board loc owner []
  (\(x, y) -> False)
  [\(x, y) -> (x + 1, y + 1),
   \(x, y) -> (x + 1, y - 1),
   \(x, y) -> (x - 1, y + 1),
   \(x, y) -> (x - 1, y - 1)]

checkersMoves :: RectBoard -> Loc -> PlayerId ->
                 [Piece] -> (Loc -> Bool) ->
                 [(Loc -> Loc)] ->
                 [RectBoardMove]
checkersMoves board loc owner promotions promote fns =
  [m |
   fn <- fns,
   let newLoc = fn loc,
   let move = DirectMove loc newLoc,
   isValidMove board move,
   m <- if promote newLoc
        then [PromoteMove newLoc piece move | piece <- promotions]
        else [move]] ++
  captureMoves board loc where
    captureMoves :: RectBoard -> Loc -> [RectBoardMove]
    captureMoves board loc =
      [m |
       fn <- fns,
       let newLoc = fn.fn $ loc,
       let move = CaptureMove (fn loc) owner $ DirectMove loc (fn (fn loc)),
       isValidMove board move,
       m <- if promote newLoc
            then [PromoteMove newLoc piece move | piece <- promotions]
            else move : [SeqMove [move, m]
                        | m <- captureMoves (doMove move board) newLoc]]

name :: Piece -> String
name (Checker _ _ _) = "checker"
name (CheckerKing _) = "checker king"

label :: Piece -> Char
label (Checker player _ _) = if player `mod` 2 == 0 then '⛀' else '⛂'
label (CheckerKing player) = if player `mod` 2 == 0 then '⛁' else '⛃'

color :: Piece -> Color
-- Add other cases
color p = [White, Green, Yellow, Blue, Magenta, Cyan]!!(owner p `div` 2)

owner :: Piece -> PlayerId
owner (Checker player _ _) = player
owner (CheckerKing player) = player
