from game import *
from checker import *
from bishop import *
from camel import *
import math

class Chesskers(Game):
    def __init__(self):
        size = 8
        super().__init__(size, 2)

    def pieces(self):
        pieces = {}
        for i in range(3):
            for j in range(0, 8, 2):
                if i == 0:
                    if j == 0:
                        pieces[i, j] = Bishop(1)
                        pieces[self.size - i - 1, j + 1] = Camel(2)
                    elif j == 6:
                        pieces[i, j] = Camel(1)
                        pieces[self.size - i - 1, j + 1] = Bishop(2)
                    else:
                        pieces[i, j] = CheckerKing(1)
                        pieces[self.size - i - 1, j + 1] = CheckerKing(2)
                elif i % 2:
                    pieces[i, j + 1] = Checker(1, 'DOWN', [CheckerKing(1), Camel(1), Bishop(1)])
                    pieces[self.size - i - 1, j] = Checker(2, 'UP', [CheckerKing(2), Camel(2), Bishop(2)])
                else:
                    pieces[i, j] = Checker(1, 'DOWN', [CheckerKing(1), Camel(1), Bishop(1)])
                    pieces[self.size - i - 1, j + 1] = Checker(2, 'UP', [CheckerKing(2), Camel(2), Bishop(2)])
        return pieces

    def lost(self, board, player_num):
        return len(board.moves(player_num)) == 0 or len([piece for piece in board.active_pieces(player_num) if piece.name == 'checker king']) == 0
