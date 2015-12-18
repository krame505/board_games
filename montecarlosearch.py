from player import *
#from heuristicminmax import *
from randomized import *

import random
import concurrent.futures
import time

def run_trial(init_board, init_turn, games = 5):
    """Play a trial game starting from the given board"""
    game = init_board.game
    #player = HeuristicMinMaxSearchPlayer(1, 'default', 5, False, False)
    player = RandomizedPlayer()
    moves = []
    result = 0
    for i in range(games):
        turn = init_turn % 2 + 1
        board = init_board
        moves.append([])
        while not board.game.lost(board, turn) and len(board.moves(turn)) != 0:
            # Get a move and update the board
            player.num = turn
            move = player.get_move(board)
            moves[-1].append(move)
            board = board.move(move)
            # Advance the turn
            turn = turn % 2 + 1
        if turn == 2:
            result += 1
    return result, moves

class MonteCarloTree:
    def __init__(self, move_state, parent = None, depth = 0, player_num = None):
        self.depth = depth
        self.parent = parent
        self.num_runs = 0
        self.num_wins = 0
        if self.parent:
            self.state = parent.state.move(move_state)
            self.move = move_state
            self.executor = parent.executor
            self.results = parent.results
            self.player_num = parent.player_num % 2 + 1
        else:
            self.state = move_state
            self.executor = concurrent.futures.ProcessPoolExecutor(max_workers = 20)
            self.results = {}
            self.player_num = player_num
        self.children = []

    def __del__(self):
        if self.parent == None:
            self.executor.shutdown()

    def __str__(self):
        return ("\t" * self.depth +
                str(self.num_wins) + "/" +
                str(self.num_runs) + 
                (": " + str(self.num_wins / self.num_runs)
                 if self.num_runs > 0
                 else "") +
                ("\n" + "".join(map(str, self.children))
                 if self.depth < 3
                 else "<children not shown>\n"))

    def __repr__(self):
        return "<Node " + str(self.num_wins) + "/" + str(self.num_runs) + ": " + str(self.num_wins / self.num_runs) + ">"
    
    def __lt__(self, other):
        return random.choice([True, False]) # TODO, order nodes with same scores

    def run_trials(self, timeout = 10, max_parallel = 10, num_games = 7):
        start = time.time()
        while time.time() - start < timeout:
            for node in self.results.keys():
                if not node.busy():
                    node.wait()
                    break
            if len(self.results) > max_parallel:
                self.wait_one()
            node = self.select()
            node.trial(num_games)
        self.wait_all()

    def wait_one(self):
        list(self.results.keys())[0].wait()

    def wait_all(self):
        for node in list(self.results.keys()):
            node.wait()

    def wait(self):
        result = self.results[self]
        self.update(result.result())
        del self.results[self]

    def busy(self):
        return self in self.results and not self.results[self].done()

    def all_busy(self):
        return self.busy() or all(node.busy() for node in self.children)

    def update(self, result):
        wins, movess = result
        for moves in movess:
            node = self
            for move in moves:
                prev_node = node
                node = MonteCarloTree(move, prev_node, prev_node.depth + 1)
                prev_node.children.append(node)
        node.backpropagate(wins, len(movess))
        
    def backpropagate(self, wins, games):
        self.num_wins += wins
        self.num_runs += games
        if self.parent:
            self.parent.backpropagate(wins, games)

    def trial(self, num_games):
        self.results[self] = self.executor.submit(run_trial, self.state, self.player_num, num_games)

    def select(self):
        if self.busy():
            self.wait()
        if len(self.children) < len(self.state.moves(self.player_num)):
            move = list(set(self.state.moves(self.player_num)) - {child.move for child in self.children})[0] # TODO: ordering
            child = MonteCarloTree(move, self, self.depth + 1)
            self.children.append(child)
            return child
        else:
            options = [(node.num_wins / node.num_runs, node) for node in self.children if not node.all_busy() and node.num_runs > 0]
            if len(options) == 0:
                options = [(node.num_wins / node.num_runs, node) for node in self.children if not node.busy() and node.num_runs > 0]
            if len(options) == 0:
                options = [(node.num_wins / node.num_runs, node) for node in self.children if node.num_runs > 0]
            if len(options) == 0:
                self.children[0].wait()
                options = [(node.num_wins / node.num_runs, node) for node in self.children if node.num_runs > 0]
            if self.player_num == 1:
                return max(options)[1].select()
            elif self.player_num == 2:
                return min(options)[1].select()

    def get_move(self):
        #print(self)
        if self.player_num == 1:
            score, node = max((node.num_wins / node.num_runs, node) for node in self.children)
        elif self.player_num == 2:
            score, node = min((node.num_wins / node.num_runs, node) for node in self.children)
        print("Score:", score)
        return node.move

class MonteCarloSearchPlayer(Player):
    """Player that uses monte tree search for scoring"""

    def __init__(self, timeout = 10, num = None):
        self.timeout = timeout
        super().__init__(num)
        
    def get_move(self, board, verbose = False):
        tree = MonteCarloTree(board, player_num = self.num)
        tree.run_trials(self.timeout)
        #print(tree)
        return tree.get_move()
