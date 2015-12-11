#!/usr/bin/python3
# Main driver for chesskers

from chesskers import *
from humanplayer import HumanPlayer
from randomized import RandomizedPlayer
from montecarlo import MonteCarloPlayer
from heuristicminmax import HeuristicMinMaxSearchPlayer
from trainedminmax import TrainedMinMaxSearchPlayer

import sys

# Generate different types of players based on name
def get_player(name):
    if name == 'human':
        return HumanPlayer()
    elif name == 'random':
        return RandomizedPlayer()
    elif name == 'mc':
        return MonteCarloPlayer()
    elif name == 'heuristic':
        return HeuristicMinMaxSearchPlayer(weights = 'default')
    elif name == 'genetic':
        return HeuristicMinMaxSearchPlayer()
    elif name == 'svm':
        return TrainedMinMaxSearchPlayer()
    else:
        sys.exit("Invalid player type: " + name)

if __name__ == '__main__':
    player1_type = 'human'
    player2_type = 'svm'
    if len(sys.argv) > 2:
        player2_type = sys.argv[2]
    if len(sys.argv) == 2:
        player2_type = sys.argv[1]
    elif len(sys.argv) > 1:
        player1_type = sys.argv[1]

    print("Playing", player1_type, "vs.", player2_type)

    player1 = get_player(player1_type)
    player2 = get_player(player2_type)

    game = Chesskers(player1, player2)
    game.play(True)
