# Player that uses monte carlo trials based on playing semi-random min-max heruristic games for scoring

from player import *
from heuristicminmax import *
import random
import multiprocessing

# Play a game starting from the given board
def run_trial(board, player_1, player_2):
    for j in range(1000):
        if board.game.lost(board, player_1.num):
            return 1
        elif len(board.moves(player_1.num)) == 0:
            return 0
        board = board.move(player_1.get_move(board))
        if board.game.lost(board, player_2.num):
            return -1
        elif len(board.moves(player_2.num)) == 0:
            return 0
        board = board.move(player_2.get_move(board))
    return 0

# Worker function unpacks arguments and calls score
def worker(arg):
    return run_trial(*arg)

# Score a board by running trials repeatedly
def score(board, num, max_depth, randomize, num_trials):
    opponent = 1 if num == 2 else 2
    player_1 = HeuristicMinMaxSearchPlayer(max_depth, 'default', randomize, False, False, opponent)
    player_2 = HeuristicMinMaxSearchPlayer(max_depth, 'default', randomize, False, False, num)
    pool = multiprocessing.Pool(10)
    result = pool.map(worker, ((board, player_1, player_2) for i in range(num_trials)))
    pool.close()
    pool.join()
    return sum(result)

class MonteCarloPlayer(Player):
    def __init__(self, max_depth = 1, randomize = 5, num_trials = 10, num = None):
        self.max_depth = max_depth
        self.randomize = randomize
        self.num_trials = num_trials
        super().__init__(num)
        
    def get_move(self, board, verbose = False):
        s, move = max((score(board.move(m), self.num, self.max_depth, self.randomize, self.num_trials), m)
                      for m in board.moves(self.num))
        return move
