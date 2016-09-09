module TrainedPlayer where
import Games
import MCTSPlayer

import Data.Maybe

import System.IO
import System.Random
import Control.Monad.Random
import Control.Concurrent.ParallelIO

distribute :: Int -> [a] -> [[a]]
distribute n elems = distributeHelp 0 elems
  where
    distributeHelp i [] = replicate n []
    distributeHelp i (h:t)
      | i < n =
        let (x, l : y) = splitAt i $ distributeHelp (i + 1) t
        in x ++ (h : l) : y
      | otherwise = distributeHelp 0 (h:t)

buildData :: Game b m =>
             FilePath -> GameState b m -> Int -> Integer -> Int -> IO ()
buildData fileName initial numParallel numTrials numData =
  do file <- openFile fileName AppendMode
     statesGen <- newStdGen
     playoutGens <- sequence (replicate numParallel newStdGen)
     parallel $
       [sequence [hPutStrLn file $ show $ evalRand (getPlayout game) gen
                 | game <- games]
       | (gen, games) <-
           zip (concat $ repeat playoutGens) $
           distribute numParallel (take numData $ trainingStates statesGen)]
     hClose file
     return ()
  where
    trainingStates gen = states ++ trainingStates newGen
      where
        playoutStates game
          | isJust $ winner game = return []
          | isDraw game = return []
          | null $ gameMoves game = error "No moves and not a win or draw"
          | otherwise =
            do move <- randSample $ gameMoves game
               states <- playoutStates (makeMove game move)
               return $ game : states
        
        (states, newGen) = runRand (playoutStates initial) gen

    getPlayout game =
      do tree <- trials numTrials $ initTree game
         return (getWins tree, features $ board game)
