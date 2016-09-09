{-# LANGUAGE MultiParamTypeClasses #-}
-- Simple demo/test

import Games

import Data.Char (isDigit)

import RandomPlayer
import HumanPlayer
import MCTSPlayer

import System.Environment

newtype NimBoard = NimBoard Int
                 deriving (Show, Read, Eq)
newtype NimMove = NimMove Int
                 deriving (Show, Read, Eq, Ord)

instance Board NimBoard NimMove where
  moves player (NimBoard count) =
    [NimMove n | n <- [1, 2], n <= count]
  
  readMove player n (NimBoard count)
    | all isDigit n = Left $ NimMove $ read n
    | otherwise = Right "Invalid move"

  showMove (NimMove n) _ = "removed " ++ show n

  doMove (NimMove n) (NimBoard count) = NimBoard $ count - n

  display (NimBoard count) = take count $ repeat '*'

instance Game NimBoard NimMove where
  initial = GameState {board=NimBoard 17, players=initPlayers 2}
  
  winner GameState {board=NimBoard 0, players=p:_} = Just p
  winner _ = Nothing

  isDraw game = False

  isActive game player = True

playerOptions :: [(String, Player NimBoard NimMove)]
playerOptions = [("random", randomPlayer),
                 ("human", humanPlayer),
                 ("mcts", mctsPlayer 1000 2)]
          
main :: IO ()
main = do args <- getArgs
          let players =
                [case lookup arg playerOptions of
                    Just player -> player
                    Nothing -> error $ "Invalid player " ++ arg
                | arg <- args] ++ repeat humanPlayer
          winner <- playGame players True
          return ()
