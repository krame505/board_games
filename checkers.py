from game import *
from checker import *
import math

class Checkers(Game):
    """Implementation of checkers game"""

    def __init__(self, size = 8):
        super().__init__(size, 2)

    def pieces(self):
        pieces = {}
        for i in range(int(math.log2(self.size))):
            for j in range(0, self.size, 2):
                if self.size % 2 != 0:
                    if i % 2:
                        if j < self.size - 1:
                            pieces[i, j + 1] = Checker(1, 'DOWN')
                            pieces[self.size - i - 1, j + 1] = Checker(2, 'UP')
                    else:
                        pieces[i, j] = Checker(1, 'DOWN')
                        pieces[self.size - i - 1, j] = Checker(2, 'UP')
                elif i % 2:
                    pieces[i, j + 1] = Checker(1, 'DOWN')
                    pieces[self.size - i - 1, j] = Checker(2, 'UP')
                else:
                    pieces[i, j] = Checker(1, 'DOWN')
                    pieces[self.size - i - 1, j + 1] = Checker(2, 'UP')

    def lost(self, board, player_num):
        return len(board.moves(player_num)) == 0
