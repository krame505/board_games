
module MinMaxPlayer where
import Games
import RectBoard

import Data.List
import Data.Maybe
import Data.Function

import Control.Parallel

minmax :: Game b m =>
          (PlayerId -> GameState b m -> Float) -> 
          (PlayerId -> GameState b m -> GameState b m -> Ordering) -> Integer ->
          PlayerId -> Integer ->
          (Float, Float) -> GameState b m ->
          (Float, m)
minmax score heuristic heuristicDepth maxPlayer depth cutoff game
  | isDraw game = (0, error "No move")
  | isJust (winner game) && fromJust (winner game) == maxPlayer =
    (read "Infinity", error "No move")
  | isJust (winner game) =
    (read "-Infinity", error "No move")
  | depth == 0 = (score maxPlayer game, error "No move")
  | turn game == maxPlayer =
      getMove orderedMoves cutoff (read "-Infinity", error "No move")
  | otherwise =
      getMove orderedMoves cutoff (read "Infinity", error "No move")
  where
    orderedMoves =
      if depth >= heuristicDepth
      then map snd $ sortBy (heuristic maxPlayer `on` fst)
           [(makeMove game move, move) | move <- (gameMoves game)]
      else (gameMoves game)
    cmp = if turn game == maxPlayer then (>=) else (<=)
    getMove (move:moves) (alpha, beta) (s, m) =
      let (s1, _) = minmax
                    score heuristic heuristicDepth
                    maxPlayer (depth - 1) (alpha, beta) (makeMove game move)
          (optScore, optMove) = if s1 `cmp` s then (s1, move) else (s, m)
      in if turn game == maxPlayer
         then if optScore >= beta
              then (optScore, optMove)
              else getMove moves (max alpha optScore, beta) (optScore, optMove)
         else if optScore <= alpha
              then (optScore, optMove)
              else getMove moves (alpha, min beta optScore) (optScore, optMove)
    getMove [] _ res = res

parallelMinMax ::
  Game b m =>
  (PlayerId -> GameState b m -> Float) -> 
  (PlayerId -> GameState b m -> GameState b m -> Ordering) -> Integer ->
  PlayerId -> Integer ->
  GameState b m ->
  (Float, m)
parallelMinMax score heuristic heuristicDepth maxPlayer depth game =
  let children =
        [minmax score heuristic heuristicDepth maxPlayer
          depth (read "-Infinity", read "Infinity")
          (makeMove game move)
        | move <- gameMoves game]
  in foldr par (maximumBy (compare `on` fst) (zip (map fst children) $ gameMoves game)) children

minMaxPlayer :: Game b m =>
                (PlayerId -> GameState b m -> Float) ->
                (PlayerId -> GameState b m -> GameState b m -> Ordering) ->
                Integer -> Integer -> Player b m
minMaxPlayer score heuristic heuristicDepth depth game =
  let (s, m) = parallelMinMax
               score heuristic heuristicDepth
               (turn game) depth game
  in do putStrLn $ "Score: " ++ show s
        return m
