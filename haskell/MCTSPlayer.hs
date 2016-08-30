
module MCTSPlayer where
import System.Random
import Control.Monad.Random
import qualified Data.Map.Strict as Map --(Map, (!), size, insert, toList, fromList, keys, elems)
import Data.Ord
import Data.Maybe
import Data.List (maximumBy)
import qualified Data.Tree
import Data.Ix

import Games

data GameTree b m = Node (GameState b m) (Map.Map m (GameTree b m)) [Int]
                  | Leaf (GameState b m)
                  | Terminal (GameState b m) (Maybe PlayerId)
                  deriving (Show, Read)

-- Get the total number of wins in a tree
getWins :: GameTree b m -> [Int]
getWins (Node _ _ w) = w
getWins (Leaf game) = replicate (numPlayers game) 0
getWins (Terminal game Nothing) = replicate (numPlayers game) 0
getWins (Terminal game (Just winner)) =
  [if winner == p then 1 else 0 | p <- range (0, numPlayers game - 1)]

-- Compute Upper Confidence Bound 1 for a node in the tree
ucb1 :: Int -> PlayerId -> GameTree b m -> Float
ucb1 0 _ _ = read "Infinity" -- None of the nodes have been tried yet
ucb1 trials player (Node game children wins) =
  (fromIntegral $ wins!!player) / (fromIntegral $ sum wins) +
  sqrt (2.0 * log (fromIntegral trials) / (fromIntegral $ sum wins))
ucb1 trials player (Leaf game) = read "Infinity" -- choose leaves with no trials first
ucb1 trials player (Terminal game Nothing) = 0
ucb1 trials player (Terminal game (Just winner))
  | winner == player = read "Infinity"
  | otherwise        = 0

-- Perform a complete playout or set of playouts and update the tree
trial :: (Game b m, RandomGen rg) =>
         GameTree b m -> Rand rg (GameTree b m)
trial (Node game children wins) =
  let weights = map (ucb1 (sum wins) (turn game)) $ Map.elems children
      cantidates = [key | (key, weight) <- zip (Map.keys children) weights,
                    weight == maximum weights]
  in do r <- getRandomR (0, length cantidates - 1)
        let key = cantidates!!r
        result <- trial $ children Map.! key
        let newChildren = Map.insert key result children
        return $ Node game newChildren $
            foldl (zipWith (+)) (replicate (numPlayers game) 0) $
            map getWins (Map.elems newChildren)
trial (Leaf game)
  | isJust $ winner game = return $ Terminal game $ winner game
  | isDraw game = return $ Terminal game Nothing
  | null $ moves (turn game) (board game) = error "No moves and not a win or draw"
  | otherwise = trial $ Node game
                (Map.fromList [(move, Leaf $ makeMove game move)
                              | move <- moves (turn game) (board game)]) $
                replicate (numPlayers game) 0
trial (Terminal game winner) = return $ Terminal game winner

-- Perform n trials on a tree
trials :: (Game b m, RandomGen rg) =>
          Integer -> GameTree b m -> Rand rg (GameTree b m)
trials 0 tree = return tree
trials n tree = do res <- trial tree
                   trials (n - 1) res

toDataTree tree@(Node game children wins) =
  Data.Tree.Node ("Node " ++ show game ++ " " ++ show wins)
  $ map (toDataTreeHelp (sum wins) $ turn game) $ Map.elems $ children
  where toDataTreeHelp trials player tree@(Node game children wins) =
          Data.Tree.Node
          ("Node " ++ show game ++ " " ++ show (ucb1 (sum wins) player tree) ++ " " ++ show wins)
          $ map (toDataTreeHelp (sum wins) $ turn game) $ Map.elems $ children
        toDataTreeHelp trials player tree = Data.Tree.Node (show tree) []
toDataTree tree = Data.Tree.Node (show tree) []

mctsPlayer :: Game b m => Player b m --GameTree b m -> 
mctsPlayer game =
  do tree <- evalRandIO $ trials 10 $ Leaf game
     --putStrLn $ Data.Tree.drawTree $ toDataTree tree
     case tree of
       Node game children wins ->
         return $
         fst $ maximumBy (comparing (\(_, child) -> ucb1 (sum wins) (turn game) child)) $
         Map.toList children
