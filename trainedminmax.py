# Min-max search player that uses a support vector machine for the scoring heuristic

from minmaxsearch import *

import random
import json
import pickle

# Convert a board to an 'input vector' list for the SVM
# Includes 1s and 0s for presence or absence of each piece for each player for each location, as well
# as total counts of each type of piece for each player
def board_to_inputs(board):
    players = [1, 2]
    pieces = ['checker', 'checker king', 'camel', 'bishop']
    result = {(player, piece): {(i, j): 0
                                for i in range(len(board))
                                for j in range(len(board))}
              for player in players for piece in pieces}
    for x, y in board.locations.values():
        piece = board[x, y]
        result[piece.player_num, piece.name][x, y] = 1
    counts = {(player, piece): sum(result[player, piece].values())
              for player in players for piece in pieces}
    return ([result[player, piece][i, j]
             for player in players
             for piece in pieces
             for i in range(len(board))
             for j in range(len(board))
             if (i + j) % 2 == 0] +
            [counts[player, piece]
             for player in players
             for piece in pieces])

class TrainedMinMaxSearchPlayer(MinMaxSearchPlayer):
    def __init__(self, max_depth = 4, scoring_svm = None, randomize = 0, parallelize = True, dynamic_depth = True, num = None):
        # If no scoring svm is provided, try to load the pickled one
        # If that fails, try to run training
        if scoring_svm == None:
            try:
                scoring_svm = 'svm.pickle'
                open(scoring_svm)
            except FileNotFoundError:
               scoring_svm = 'training.json'

        # If the provided svm is a string ending with '.pickle', then load it from that file
        if isinstance(scoring_svm, str) and scoring_svm.endswith('.pickle'):
            print("Loading svm...")
            scoring_svm = pickle.load(open(scoring_svm, 'rb'))
        # If the provided svm is a string ending with '.json', then load training data from
        # that file and train a new SVM.  Dump the generated SVM to svm.pickle
        elif isinstance(scoring_svm, str) and scoring_svm.endswith('.json'):
            print("Building svm...")
            from sklearn import svm
            training_data = json.loads(open(scoring_svm).read())
            scoring_svm = svm.SVR()
            scoring_svm.fit(*training_data)
            pickle.dump(scoring_svm, open('svm.pickle', 'wb'))

        self.scoring_svm = scoring_svm
        super().__init__(max_depth, randomize, parallelize, dynamic_depth, num)

    # Appply the svm to the inputs
    def score(self, board):
        res = self.scoring_svm.predict([board_to_inputs(board)])[0]
        if self.num == 1:
            return res
        else:
            return -res

    # Node ordering heruistic, apply the SVM for the first 2 levels.  After that running the SVM is
    # slower than the speed-up over the default heuristic of the number of pieces each player has
    def heuristic(self, board, depth):
        if depth > self.max_depth - 2:
            return self.score(board)
        else:
            return super().heuristic(board, depth)
