module RandomPlayer where
import System.Random

import Games

randomPlayer :: GameState gs m => Player gs m
randomPlayer game =
  do gen <- getStdGen
     let (num, newGen) = next gen
     setStdGen newGen
     let options = moves (turn game) (state game)
     return $ options!!(num `mod` length options)
