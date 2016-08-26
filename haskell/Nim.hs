{-# LANGUAGE MultiParamTypeClasses #-}
module Nim where
-- Simple demo/test

import Games

import Data.Ix (range)
import Data.Char (isDigit)

newtype NimBoard = NimBoard Int
                 deriving Eq
newtype NimMove = NimMove Int
                 deriving (Show, Eq)

instance GameState NimBoard NimMove where
  moves player (NimBoard count) =
    [NimMove n | n <- range (1, 2), n <= count]
  
  readMove player n (NimBoard count) | all isDigit n = Left $ NimMove $ read n
                                     | otherwise = Right "Invalid move"

  doMove (NimMove n) (NimBoard count) = NimBoard $ count - n

instance Show NimBoard where
  show (NimBoard count) = take count $ repeat '*'

nim :: Int -> Game NimBoard NimMove
nim count = initGame
  (NimBoard count)
  2
  (\(NimBoard count) players -> if count == 0
                                then Just $ head players
                                else Nothing)
  (\_ _ -> False)
  (\_ _ -> True)
