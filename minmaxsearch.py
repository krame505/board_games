from player import *
from board import *

from abc import ABCMeta, abstractmethod
import random
import multiprocessing
import time
from queue import PriorityQueue, Queue

def minmax(state, depth, score, heuristic, terminal, player1, player2, parallelize = True):
    """minmax wrapper, also performs first max-search parallelized if enabled"""

    # Call max_value if no parallelization
    if not parallelize:
        return max_value(state, float('-inf'), float('inf'), depth, score, heuristic, terminal, player1, player2)

    # Search base case
    opt = Move(-1, -1, -1, -1, [], "No move:\n" + str(state))
    if depth <= 0 or terminal(state):
        return score(state), opt

    # Initialize the parallel process pool
    pool = multiprocessing.Pool(10)

    alpha = float('-inf')
    beta = float('inf')

    # Manage processes
    # Moves are sorted by the ordering heuristic in a priority queue
    inputs = PriorityQueue()
    for move in state.moves(player1):
        inputs.put((-heuristic(state.move(move), depth), move))

    # Store the result handles in a queue
    outputs = Queue()

    # Submits the next move from the priority queue to the pool, and puts the result handle and move
    # in the outputs queue
    def submit_next():
        s, move = inputs.get()
        outputs.put((pool.apply_async(min_value, args = (state.move(move), alpha, beta, depth - 1, score, heuristic, terminal, player1, player2)), move))

    # Retrieves a result handle from the outputs queue, waits for it to finish, and returns the score and move
    def get_next():
        res, move = outputs.get()
        return res.get()[0], move

    # Submit the first 8 moves to start
    for i in range(8):
        if inputs.empty():
            break
        submit_next()

    # Perform the first max-value computation
    v = float('-inf')
    # While there are moves remaining to submit or results to be processed
    while not inputs.empty() or not outputs.empty():
        # Submit the next move if there are any left
        if not inputs.empty():
            submit_next()
        # Retrieve the next result and move from the outputs, check if it is optimal
        v, opt = max((v, opt), get_next())
        # Check for beta cutoff and update alpha
        if v >= beta:
            break
        alpha = max(alpha, v)

    # Destruct the pool
    pool.close()
    pool.join()

    return v, opt

def max_value(state, alpha, beta, depth, score, heuristic, terminal, player1, player2):
    """Max portion of minmax search"""

    # Search base case
    opt = Move(-1, -1, -1, -1, [], "No move:\n" + str(state))
    if depth <= 0 or terminal(state):
        return score(state), opt

    v = float('-inf')
    # Iterate through moves in sorted order according to the heuristic
    for s, move in sorted([(heuristic(state.move(m), depth), m) for m in state.moves(player1)]):
        # Compute the next min value, check if it is optimal
        s1, m = min_value(state.move(move), alpha, beta, depth - 1, score, heuristic, terminal, player1, player2)
        v, opt = max((v, opt), (s1, move))
        # Check for beta cutoff and update alpha
        if v >= beta:
            break
        alpha = max(alpha, v)
    return v, opt

def min_value(state, alpha, beta, depth, score, heuristic, terminal, player1, player2):
    """Min portion of minmax search"""
    # Search base case
    opt = Move(-1, -1, -1, -1, [], "No move:\n" + str(state))
    if depth <= 0 or terminal(state):
        return score(state), opt

    v = float('inf')
    # Iterate through moves in sorted order according to the heuristic
    for s, move in sorted([(heuristic(state.move(m), depth), m) for m in state.moves(player2)], reverse = True):
        # Compute the next max value, check if it is optimal
        s1, m = max_value(state.move(move), alpha, beta, depth - 1, score, heuristic, terminal, player1, player2)
        # Check for alpha cutoff and update beta
        v, opt = min((v, opt), (s1, move))
        if v <= alpha:
            break
        beta = min(beta, v)
    return v, opt

class MinMaxSearchPlayer(Player):
    """Abstract base class for minmax search players, children have different scoring functions"""
    __metaclass__ = ABCMeta

    def __init__(self, max_depth = 5, randomize = 0, parallelize = True, dynamic_depth = True, num = None):
        self.max_depth = max_depth
        self.randomize = randomize
        self.parallelize = parallelize
        self.dynamic_depth = dynamic_depth
        super().__init__(num)

    @abstractmethod
    def score(self, board):
        """Abstract terminal scoring function"""
        raise NotImplementedError

    def heuristic(self, board, depth):
        """Node ordering heurisitic, should usually be overridden.  Can be the same as score() if fast enough
        Depth is provided so a better but slower method can be used higher in the tree, then switch to a
        slower one further down where it doesn't matter.  By default, it is just the difference in the
        number of pieces each player has"""
        return len(board.active_pieces(self.num)) - len(board.active_pieces(self.opponent))

    def calc_score(self, board):
        """Wrapper for score function that adds checks for immediate win/loss and randomization"""
        if board.game.lost(board, self.num):
            return float('-inf')
        elif board.game.lost(board, self.opponent):
            return float('inf')
        else:
            return self.score(board) + random.randint(-self.randomize, self.randomize)

    def terminal(self, board):
        """Checks if the board's game has been won"""
        return board.game.lost(board, self.num) or board.game.lost(board, self.opponent)

    def get_move(self, board, verbose = False):
        """Implements alpha-beta search"""

        self.opponent = 1 if self.num == 2 else 2

        start = time.time()
        score, move = minmax(board, self.max_depth, self.calc_score, self.heuristic, self.terminal, self.num, self.opponent, self.parallelize)
        elapsed = time.time()
        elapsed = elapsed - start
        
        if self.dynamic_depth:
            if elapsed < 3:
                self.max_depth += 1
                if verbose:
                    print("Took", "{0:.2f}".format(elapsed), "sec, increasing depth to", self.max_depth)
                start = time.time()
                score, move = minmax(board, self.max_depth, self.calc_score, self.heuristic, self.terminal, self.num, self.opponent, self.parallelize)
                elapsed = time.time()
                elapsed = elapsed - start
            if elapsed > 15:
                self.max_depth -= 1
                if verbose:
                    print("Took", "{0:.2f}".format(elapsed), "sec, decreasing depth to", self.max_depth)

        if verbose:
            for x, y in move.captured:
                print(self, "Captured", board[x, y], "at", chr(ord('A') + x) + str(y + 1))
            print("Score:", score)

        self.prev_score = score
        return move
