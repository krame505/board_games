from game import *
from checker import *
import math

class Checkers(Game):
    def __init__(self, player1, player2, size = 8):
        player1.num = 1
        player2.num = 2
        players = [player1, player2]
        pieces = {}
        for i in range(int(math.log2(size))):
            for j in range(0, size, 2):
                if size % 2 != 0:
                    if i % 2:
                        if j < size - 1:
                            pieces[i, j + 1] = Checker(player1, 'DOWN')
                            pieces[size - i - 1, j + 1] = Checker(player2, 'UP')
                    else:
                        pieces[i, j] = Checker(player1, 'DOWN')
                        pieces[size - i - 1, j] = Checker(player2, 'UP')
                elif i % 2:
                    pieces[i, j + 1] = Checker(player1, 'DOWN')
                    pieces[size - i - 1, j] = Checker(player2, 'UP')
                else:
                    pieces[i, j] = Checker(player1, 'DOWN')
                    pieces[size - i - 1, j + 1] = Checker(player2, 'UP')
        super().__init__(size, pieces, players)

    def lost(self, board, player_num):
        return len(board.moves(player_num)) == 0
