module RandomPlayer where
import System.Random

import Games

randomPlayer :: Player gs m
randomPlayer game =
  do gen <- getStdGen
     let (num, newGen) = next gen
     setStdGen newGen
     let options = actions game
     return $ options!!(num `mod` length options)
