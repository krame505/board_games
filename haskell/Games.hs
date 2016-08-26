{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses, FunctionalDependencies #-}
module Games(
  Game, PlayerId, Player,
  initGame,
  state, players,
  winner, isDraw, isActive, turn, advance,
  GameState,
  moves, readMove, doMove,
  playGame) where

import Data.Maybe
import Data.Ix

data Game gs m = GameState gs m =>
  Game {state :: gs,

        -- List of remaining players in order of next turn
        players :: [PlayerId],
        winnerState :: gs -> [PlayerId] -> Maybe PlayerId,
        isDrawState :: gs -> [PlayerId] -> Bool,
        isActiveState :: gs -> PlayerId -> Bool}
  
type PlayerId = Int
type Player gs m = Game gs m -> IO m

initGame :: GameState gs m =>
            gs -> Int ->
            (gs -> [PlayerId] -> Maybe PlayerId) ->
            (gs -> [PlayerId] -> Bool) ->
            (gs -> PlayerId -> Bool) ->
            Game gs m
initGame initial numPlayers winnerState isDrawState isActiveState =
  Game {state=initial,
        players=range (0, numPlayers - 1),
        winnerState=winnerState,
        isDrawState=isDrawState,
        isActiveState=isActiveState}

-- Return Just player if the game has been won
winner :: Game gs m -> Maybe PlayerId
winner game@Game {state=gs, players=ps} = winnerState game gs ps

-- Return true if the game is a draw
isDraw :: Game gs m -> Bool
isDraw game@Game {state=gs, players=ps} = isDrawState game gs ps

-- Return true if player is still in the game
isActive :: Game gs m -> PlayerId -> Bool
isActive game@Game {state=gs} player = isActiveState game gs player

turn :: Game gs m -> PlayerId
turn Game {players=p:_} = p
{-
actions :: Game gs m -> [Game gs m]
actions game@Game {players=ps} | isJust $ winner game = []
                               | isDraw game = []
                               | not $ any (isActive game) ps = []
actions game@Game {state=gs, players=p:ps} | isActive game p =
  [game {state=doMove move gs, players=ps ++ [p]} | move <- moves p gs]
                                           | otherwise       =
    actions game {players=ps ++ [p]}-}

advance :: Game gs m -> m -> Game gs m
advance game@Game {state=gs, players=p1:p2:ps} move
  | isActive game p2 = game {state=doMove move gs, players=p2:ps ++ [p1]}
  | otherwise = advance game {players= p2:ps ++ [p1]} move

class (Show gs, Eq gs) => GameState gs m | gs -> m where
  moves :: PlayerId -> gs -> [m]
  readMove :: PlayerId -> String -> gs -> Either m String
  doMove :: m -> gs -> gs

-- Driver
-- TODO: Show in type signature shouldn't be needed?
playGame :: (Show gs, GameState gs m) => Game gs m -> [Player gs m] -> Bool -> IO (Maybe PlayerId)
playGame game playerAIs verbosity =
  if length (zip playerAIs $ players game) < length (players game) -- playerAIs can be infinite
  then fail "Not enough players"
  else if null $ moves (turn game) (state game)
       then case winner game of
         Just winner -> do if verbosity
                             then putStrLn $ "\nPlayer " ++ show (winner + 1) ++ " won!"
                             else return ()
                           return $ Just winner
         Nothing -> if isDraw game
                    then return Nothing
                    else error "No actions and not a draw or win"
       else do if verbosity
                 then do putStrLn $ "\nPlayer " ++ (show $ turn game + 1) ++ "'s turn"
                         putStrLn $ show (state game)
                 else return ()
               move <- playerAIs!!(turn game) $ game
               playGame (advance game move) playerAIs verbosity
