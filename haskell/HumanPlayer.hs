module HumanPlayer where
import System.IO

import Games

humanPlayer :: Game b m => Player b m
humanPlayer game =
  do --putStrLn $ "Valid moves: " ++ show (moves (turn game) (state game))
     putStr $ "Move for player " ++ show (turn game + 1) ++ ": "
     hFlush stdout
     moveTxt <- getLine
     case readMove (turn game) moveTxt (board game) of
       Left move -> 
         if elem move $ moves (turn game) (board game)
         then return move
         else do putStrLn $ "Invalid move"-- ++ show move
                 humanPlayer game
       Right msg ->
         do putStrLn msg
            humanPlayer game
     
