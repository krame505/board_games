from player import *
from board import *

import random

class RandomizedPlayer(Player):
    def get_move(self, board, verbose = False):
        return random.choice(board.moves(self.num))
