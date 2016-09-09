{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, StandaloneDeriving #-}
module Games(
  GameState(..), Game, Board, PlayerId, Player,
  initial, winner, isDraw, isActive,
  moves, readMove, showMove, doMove, display, features,
  initPlayers, turn, numPlayers, gameMoves, makeMove,
  playGame) where

--import Data.Maybe
import Data.Ix

data GameState b m =
  GameState {board :: b,
             -- List of remaining players in order of next turn
             players :: [PlayerId]}

deriving instance Board b m => Show (GameState b m)
deriving instance Board b m => Read (GameState b m)

class (Board b m) => Game b m where
  -- Initial state
  initial :: GameState b m

  -- Return Just player if the game has been won
  winner :: GameState b m -> Maybe PlayerId

  -- Return true if the game is a draw
  isDraw :: GameState b m -> Bool

  -- Return true if player is still in the game
  isActive :: GameState b m -> PlayerId -> Bool

  -- Generate a representation of the board sutable for learning
  features :: b -> [Int]

class (Show b, Read b, Eq b,
       Show m, Read m, Eq m, Ord m) => Board b m | b -> m where
  moves :: PlayerId -> b -> [m]
  readMove :: PlayerId -> String -> b -> Either m String
  showMove :: m -> b -> String
  doMove :: m -> b -> b
  
  display :: b -> String
  
type PlayerId = Int
type Player b m = GameState b m -> IO m

initPlayers :: Int -> [PlayerId]
initPlayers numPlayers = [0..numPlayers - 1]

turn :: GameState b m -> PlayerId
turn GameState {players=p:_} = p

numPlayers :: GameState b m -> Int
numPlayers GameState {players=ps} = length ps

gameMoves :: Game b m => GameState b m -> [m]
gameMoves game = moves (turn game) $ board game

makeMove :: Game b m => GameState b m -> m -> GameState b m
makeMove game@GameState {board=b, players=p1:p2:ps} move
  | isActive game p2 = game {board=doMove move b, players=p2:ps ++ [p1]}
  | otherwise = makeMove game {players=p2:ps ++ [p1]} move

-- Driver
-- TODO: Show in type signature shouldn't be needed?
playGame :: (Show b, Game b m) => [Player b m] -> Bool -> IO (Maybe PlayerId)
playGame playerAIs verbosity = doPlayGame initial where
  --doPlayGame :: GameState b m -> IO (Maybe PlayerId)
  doPlayGame game =
    if length (zip playerAIs $ players game) < numPlayers game -- playerAIs can be infinite
    then fail "Not enough players"
    else if null $ moves (turn game) (board game)
         then case winner game of
           Just winner ->
             do if verbosity
                  then do putStrLn $ "\nPlayer " ++ show (winner + 1) ++ " won!"
                          putStrLn $ display (board game)
                  else return ()
                return $ Just winner
           Nothing -> if isDraw game
                      then do if verbosity
                                then putStrLn $ "\nDraw!"
                                else return ()
                              return Nothing
                      else error "No actions and not a draw or win"
         else do if verbosity
                   then do putStrLn $ "\nPlayer " ++ (show $ turn game + 1) ++ "'s turn"
                           putStrLn $ display (board game)
                   else return ()
                 move <- playerAIs!!(turn game) $ game
                 if verbosity
                   then do putStrLn $ "\nPlayer " ++ (show $ turn game + 1) ++ " " ++
                             showMove move (board game)
                   else return ()
                 doPlayGame (makeMove game move)
