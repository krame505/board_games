#!/usr/bin/python3
"""Generates training data for the SVM player by performing monte carlo simulations"""

from chesskers import *
from randomized import *
import montecarlo
import trainedminmax

import sys
import random
import json

if __name__ == '__main__':
    # Initialization
    # Read the number of training cases to generate and any already in the file
    size = 10000
    if len(sys.argv) > 1:
        size = int(sys.argv[1])
    print("Generating", size, "training cases...")
    training_file = 'training.json'
    try:
        training_data = json.loads(open(training_file).read())
    except FileNotFoundError:
        training_data = [[], []]

    # Trim the training data sets if they are too large
    training_data[0] = training_data[0][:size]
    training_data[1] = training_data[1][:size]

    # Generate new training sets until the size target is reached
    p1 = RandomizedPlayer(1)
    p2 = RandomizedPlayer(2)
    game = Chesskers()
    board = game.board
    while len(training_data[0]) < size:
        # Calculate the score of the board with monte carlo trials, add it to the training pool
        training_data[0].append(trainedminmax.board_to_inputs(board))
        training_data[1].append(montecarlo.score(board, 1, 2, 5, 50)) # score(board, 1, 1, 5, 50)

        # Perform one random turn for each player
        board = board.move(p1.get_move(board))
        if game.lost(board, 2) or len(board.moves(1)) == 0 or len(board.moves(2)) == 0:
            print("Finished game")
            board = game.board
        board = board.move(p2.get_move(board))
        if game.lost(board, 1) or len(board.moves(1)) == 0 or len(board.moves(2)) == 0:
            print("Finished game")
            board = game.board

        # Every 10 elements, print the status and save to the training data file
        if len(training_data[0]) % 10 == 0:
            print("Generated", len(training_data[0]), "training cases")
            open(training_file, 'w').write(json.dumps(training_data))

    # Write to the file
    print("Finished")
    open(training_file, 'w').write(json.dumps(training_data))
