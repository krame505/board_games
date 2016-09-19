
from minmaxsearch import *
import random
import json

class HeuristicMinMaxSearchPlayer(MinMaxSearchPlayer):
    """Min-max search player that uses pre-coded weights for indices and piece types for the heuristic"""
    def __init__(self, max_depth = 4, weights = 'scoring.json', randomize = 0, parallelize = True, dynamic_depth = True, num = None):
        if weights == 'default':
            checker_weights = [[4, 4, 4, 4, 4, 4, 4, 4],
                               [4, 3, 3, 3, 3, 3, 3, 4],
                               [4, 3, 2, 2, 2, 2, 3, 4],
                               [4, 3, 2, 1, 1, 2, 3, 4],
                               [4, 3, 2, 1, 1, 2, 3, 4],
                               [4, 3, 2, 2, 2, 2, 3, 4],
                               [4, 3, 3, 3, 3, 3, 3, 4],
                               [4, 4, 4, 4, 4, 4, 4, 4]]
            unit_weights = [[1, 1, 1, 1, 1, 1, 1, 1],
                            [1, 1, 1, 1, 1, 1, 1, 1],
                            [1, 1, 1, 1, 1, 1, 1, 1],
                            [1, 1, 1, 1, 1, 1, 1, 1],
                            [1, 1, 1, 1, 1, 1, 1, 1],
                            [1, 1, 1, 1, 1, 1, 1, 1],
                            [1, 1, 1, 1, 1, 1, 1, 1],
                            [1, 1, 1, 1, 1, 1, 1, 1]]
            weights = {"camel": (unit_weights, [14]),
                       "checker": (checker_weights, [1]),
                       "bishop": (unit_weights, [18]),
                       "checker king": (unit_weights, [6, 4])}
        elif isinstance(weights, str):
            weights = json.loads(open(weights).read())[0]
        self.weights = weights
        super().__init__(max_depth, randomize, parallelize, dynamic_depth, num)

    def heuristic(self, board, depth):
        def score_player(player_num):
            result = 0
            # Stores the number of pieces of each type
            piece_count = {name: len([piece for piece in board.active_pieces(player_num)
                                      if piece.name == name])
                           for name in self.weights.keys()}

            # Compute the total value of each piece found so far
            # Value is computed based on a weight for each index for each type of piece and absolute
            # value for each piece, which can be different depending on the number of that piece
            # encountered so far
            for piece in board.active_pieces(player_num):
                x, y = board.location(piece)
                value = (self.weights[piece.name][1][piece_count[piece.name]]
                         if piece_count[piece.name] < len(self.weights[piece.name][1])
                         else self.weights[piece.name][1][len(self.weights[piece.name][1]) - 1])
                result += value * self.weights[piece.name][0][x][y]
            return result

        return score_player(self.num) - score_player(self.opponent)

    def score(self, board):
        return self.heuristic(board, 0)
