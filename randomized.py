from player import *
from board import *

import random

class RandomizedPlayer(Player):
    """Player that makes random moves"""

    def __init__(self, num = None):
        super().__init__(num)

    def get_move(self, board, verbose = False):
        return random.choice(board.moves(self.num))
