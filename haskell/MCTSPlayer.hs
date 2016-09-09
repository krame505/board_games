
module MCTSPlayer where
import System.Random
import Control.Monad.Random
import Control.Monad
import Control.Parallel
--import Control.Concurrent.ParallelIO
import qualified Data.Map.Strict as Map --(Map, (!), size, insert, toList, fromList, keys, elems)
import Data.Ord
import Data.Maybe
import Data.List
import qualified Data.Tree
import Data.Ix
import Data.Function

import Games

data GameTree b m = Node (GameState b m) (Map.Map m (GameTree b m)) Int [Int]
                  | Leaf (GameState b m) Int
                  | Terminal (GameState b m) (Maybe PlayerId)
                  deriving (Show, Read)

-- Get the total number of wins in a tree
getWins :: GameTree b m -> [Int]
getWins (Node _ _ _ w) = w
getWins (Leaf game _) = replicate (numPlayers game) 0
getWins (Terminal game Nothing) = replicate (numPlayers game) 0
getWins (Terminal game (Just winner)) =
  [if winner == p then 1 else 0 | p <- [0..numPlayers game - 1]]

-- Compute Upper Confidence Bound 1 for a node in the tree
ucb1 :: Int -> PlayerId -> GameTree b m -> Float
ucb1 0 _ _ = read "Infinity" -- None of the nodes have been tried yet, so it doesn't matter
ucb1 parentTrials player (Node game children trials wins) =
  (fromIntegral $ wins!!player) / (fromIntegral $ sum wins) +
  sqrt (2.0 * log (fromIntegral parentTrials) / fromIntegral trials)
ucb1 _ player (Leaf game 0) = read "Infinity" -- choose leaves with no trials first
ucb1 parentTrials player (Leaf game trials) =
  sqrt (2.0 * log (fromIntegral parentTrials) / fromIntegral trials)
ucb1 _ player (Terminal game Nothing) = 0
ucb1 _ player (Terminal game (Just winner))
  | winner == player = read "Infinity"
  | otherwise        = 0

-- Helper function
randSample :: RandomGen g => [a] -> Rand g a
randSample [] = error "Can't sample an empty list"
randSample cantidates = do r <- getRandomR (0, length cantidates - 1)
                           return $ cantidates !! r

-- Select a node from the tree for which to perform a playout
-- Returns a tuple (updated tree, chosen node, path to the chosen node)
select :: (Game b m, RandomGen g) =>
          GameTree b m -> Rand g (GameTree b m, GameState b m, [m])
select tree@(Node game children trials wins) =
  let weights = map (ucb1 (sum wins) (turn game)) $ Map.elems children
      cantidates = [key | (key, weight) <- zip (Map.keys children) weights,
                    weight == maximum weights]
  in do when (null $ Map.elems children) $
          error $ "Node with no children:\n" ++ drawTree tree
        when (null cantidates) $
          error $ show weights ++ "\n" ++ drawTree tree
        move <- randSample cantidates
        (newTree, chosenNode, moves) <- select $ children Map.! move
        let newChildren = Map.insert move newTree children
        return (Node game newChildren (trials + 1) wins, chosenNode, move:moves)
select (Leaf game trials) = return (Leaf game (trials + 1), game, [])
select (Terminal game winner) = return (Terminal game winner, game, [])

-- Perform a playout from the given state
-- Return Just player if won by player or Nothing if a draw
playout :: (Game b m, RandomGen g) => GameState b m -> Rand g (Maybe PlayerId)
playout game
  | isJust $ winner game = return $ winner game
  | isDraw game = return $ Nothing
  | null $ gameMoves game = error "No moves and not a win or draw"
  | otherwise =
      do move <- randSample $ gameMoves game
         playout (makeMove game move)

-- Backpropagate the result of the playout for a location in the tree and player id
update :: Game b m => GameTree b m -> [m] -> Maybe PlayerId -> GameTree b m
update (Node game children trials wins) (move:moves) (Just winner) =
    let newTree = update (children Map.! move) moves (Just winner)
        newChildren = Map.insert move newTree children
        newWins = [if p == winner then n + 1 else n
                  | (n, p) <- zip wins [0..]]
    in Node game newChildren trials newWins
update (Node game children trials wins) (move:moves) Nothing =
    let newTree = update (children Map.! move) moves Nothing
        newChildren = Map.insert move newTree children
    in Node game newChildren trials wins
update (Node game children trials wins) [] (Just winner) =
  let newWins = [if p == winner then n + 1 else n
                | (n, p) <- zip wins [0..]]
  in Node game children trials newWins
update (Node game children trials wins) [] Nothing =
  Node game children trials wins
update (Leaf game trials) [] result
  | isJust (winner game) || isDraw game = Terminal game result
  | null $ gameMoves game = error "No moves and not a win or draw"
  | otherwise =
    let wins = case result of
          Just winner ->
            [if p == winner then 1 else 0
            | p <- [0..length (players game) - 1]]
          Nothing -> replicate (numPlayers game) 0
        children = Map.fromList [(move, Leaf (makeMove game move) 0)
                                | move <- gameMoves game]
    in Node game children trials wins
update (Terminal game winner) [] result
  | result == winner = (Terminal game winner)

-- Make an initial tree for a state
initTree :: Game b m => GameState b m -> GameTree b m
initTree game = Leaf game 0

-- Perform n trials on a tree
trials :: (Game b m, RandomGen g) => Integer -> GameTree b m -> Rand g (GameTree b m)
trials 0 tree = return tree
trials n tree =
  do (newTree, game, moves) <- select tree
     result <- playout game
     trials (n - 1) $ update newTree moves result

parallelTrials :: Game b m => Integer -> Int -> GameTree b m -> IO (GameTree b m)
parallelTrials n parallel tree = doTrials n [] tree
  where
    doTrials 0 [] tree = return tree
    doTrials n updates tree | n > 0 && length updates < parallel =
      do --putStrLn $ show n
         (newTree, game, moves) <- evalRandIO $ select tree
         gen <- newStdGen
         let result = evalRand (playout game) gen
         par result $ doTrials (n - 1) (updates ++ [(moves, result)]) newTree
    doTrials n ((moves, result):rest) tree = 
      doTrials n rest $ update tree moves result

drawTree :: Game b m => GameTree b m -> String
drawTree tree = Data.Tree.drawTree $ toDataTree tree
  where
    toDataTree tree@(Node game children trials wins) =
      Data.Tree.Node
      ("Node (" ++ show game ++ ") "
       ++ show trials ++ " "
       ++ show wins ++ " "
       ++ "<top node>")
      $ map (toDataTreeHelp (sum wins) $ turn game) $ Map.elems $ children
    toDataTree tree = Data.Tree.Node (show tree) []
    
    toDataTreeHelp parentTrials player tree@(Node game children trials wins) =
      Data.Tree.Node
      ("Node (" ++ show game ++ ") "
       ++ show trials ++ " "
       ++ show wins ++ " "
       ++ show (ucb1 parentTrials player tree))
      $ map (toDataTreeHelp (sum wins) $ turn game) $ Map.elems $ children
    toDataTreeHelp parentTrials player tree = Data.Tree.Node (show tree) []

mctsPlayer :: Game b m => Integer -> Int -> Player b m --GameTree b m -> 
mctsPlayer n parallel game =
  do {-tree@(Node _ children _ _) <- evalRandIO $
       do (tree1, game1, moves1) <- select $ Leaf game 0
          result1 <- playout game1
          (tree2, game2, moves2) <- select $ tree1
          result2 <- playout game2
          let tree3 = update tree2 moves1 result1
          let tree4 = update tree3 moves2 result2
          return tree4-}
    
     tree@(Node _ children numTrials _) <- parallelTrials n parallel $ initTree game
     --putStrLn $ drawTree tree
     let (result, resultTree) =
           maximumBy (comparing $ ucb1 1 (turn game) . snd) $
           Map.toList children
     putStrLn $ "Confidence: " ++ show (ucb1 1 (turn game) resultTree)
     return result
