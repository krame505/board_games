from player import *
#from heuristicminmax import *
from randomized import *

import random
from math import sqrt, log
import concurrent.futures
import time

def run_trial(init_board, turn):
    """Play a trial game starting from the given board"""
    game = init_board.game
    #player = HeuristicMinMaxSearchPlayer(1, 'default', 5, False, False)
    player = RandomizedPlayer()
    moves = []
    board = init_board
    while not board.game.lost(board, turn) and len(board.moves(turn)) != 0:
        # Get a move and update the board
        player.num = turn
        move = player.get_move(board)
        moves.append(move)
        board = board.move(move)
        # Advance the turn
        turn = turn % 2 + 1
    return turn, moves

class MonteCarloTree:
    def __init__(self, move_state, parent = None, depth = 0, bias = 0.5, player_num = None):
        self.depth = depth
        self.parent = parent
        self.num_runs = 0
        self.num_wins = 0
        if self.parent:
            self.state = parent.state.move(move_state)
            self.move = move_state
            self.executor = parent.executor
            self.player_num = parent.player_num % 2 + 1
            self.bias = parent.bias
        else:
            self.state = move_state
            self.executor = concurrent.futures.ProcessPoolExecutor(max_workers = 20)
            self.player_num = player_num
            self.bias = bias
        self.children = []

    def __del__(self):
        if self.parent == None:
            self.executor.shutdown()

    def __str__(self):
        depth_limit = 1
        return ("\t" * self.depth +
                str(self.num_wins) + "/" +
                str(self.num_runs) + 
                (": " + str(self.num_wins / self.num_runs)
                 if self.num_runs > 0
                 else "") +
                ("\n" + "".join(map(str, self.children))
                 if self.depth < depth_limit
                 else "<children not shown>\n"))

    def __repr__(self):
        return "<Node " + str(self.num_wins) + "/" + str(self.num_runs) + ": " + str(self.num_wins / self.num_runs) + ">"
    
    def __lt__(self, other):
        return random.choice([True, False]) # TODO, order nodes with same scores

    def run_trials(self, timeout = 10, num_games = 20):
        start = time.time()
        while time.time() - start < timeout:
            node = self.select()
            if node:
                node.trial(num_games)
            else:
                print("Search exaustive")
                break

    def update(self, result):
        winner, moves = result
        node = self
        for move in moves:
            prev_node = node
            node = MonteCarloTree(move, prev_node, prev_node.depth + 1)
            prev_node.children.append(node)
        node.backpropagate(winner)
        
    def backpropagate(self, winner):
        if winner == self.player_num:
            self.num_wins += 1
        self.num_runs += 1
        if self.parent:
            self.parent.backpropagate(winner)

    def trial(self, num_games):
        results = [self.executor.submit(run_trial, self.state, self.player_num) for i in range(num_games)]
        for result in results:
            self.update(result.result())

    def ucb1(self):
        return self.num_wins / self.num_runs + sqrt(2 * log(self.parent.num_runs) / self.num_runs)

    def select(self):
        if len(self.state.moves(self.player_num)) == 0:
            return None
        elif len(self.children) < len(self.state.moves(self.player_num)):
            move = list(set(self.state.moves(self.player_num)) - {child.move for child in self.children})[0] # TODO: ordering
            child = MonteCarloTree(move, self, self.depth + 1)
            self.children.append(child)
            return child
        else:
            return max([(node.ucb1(), node) for node in self.children])[1].select()

    def get_move(self):
        score, node = max((node.num_wins / node.num_runs, node) for node in self.children)
        print("Score:", score)
        return node.move

class MonteCarloSearchPlayer(Player):
    """Player that uses monte tree search for scoring"""

    def __init__(self, timeout = 10, num = None):
        self.timeout = timeout
        self.tree = None
        super().__init__(num)
        
    def get_move(self, board, verbose = False):
        #if self.tree:
        #    child_boards = {child.state: child for child in self.tree.children}
        #    if board in child_boards and self.num == child_boards[board].player_num:
        #        tree = child_boards[board].player_num
        #if not self.tree:
        self.tree = MonteCarloTree(board, player_num = self.num)
        self.tree.run_trials(self.timeout)
        #print(self.tree)
        return self.tree.get_move()
