{-# LANGUAGE ExistentialQuantification #-}
import Games
import Checkers
import Nim

import RandomPlayer
import HumanPlayer

import System.Environment

data GameEntry =
  forall gs m. (GameState gs m, Show m, Eq m) =>
  GameEntry (Game gs m) [(String, Player gs m)]

games :: [(String, GameEntry)]
games = [("nim", GameEntry
                 (nim 17)
                 [("random", randomPlayer),
                  ("human", humanPlayer)]),
         ("checkers", GameEntry
                      checkers
                      [("random", randomPlayer),
                       ("human", humanPlayer)])]

main :: IO ()
main = do args <- getArgs
          if null args
            then fail "Expected game name"
            else return ()
          GameEntry game possiblePlayers <- case lookup (head args) games of
            Just item -> return item
            Nothing -> fail $ "Invalid game " ++ (head args)
          let players =
                [case lookup arg possiblePlayers of
                    Just player -> player
                    Nothing -> error $ "Invalid player " ++ arg
                | arg <- tail args] ++ repeat humanPlayer
          winner <- playGame game players True
          return ()
          
