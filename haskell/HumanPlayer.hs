module HumanPlayer where
import System.IO

import Games

humanPlayer :: (GameState gs m, Show m, Eq m) => Player gs m
humanPlayer game =
  do putStr $ "Move for player " ++ show (turn game + 1) ++ ": "
     hFlush stdout
     moveTxt <- getLine
     case readMove (turn game) moveTxt (state game) of
       Left move -> 
         if elem move $ moves (turn game) (state game)
         then return move
         else do putStrLn $ "Invalid move" ++ show move
                 putStrLn $ "Valid moves: " ++ show (moves (turn game) (state game))
                 humanPlayer game
       Right msg ->
         do putStrLn msg
            humanPlayer game
     
