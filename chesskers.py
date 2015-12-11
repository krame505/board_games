from game import *
from checker import *
from bishop import *
from camel import *
import math

class Chesskers(Game):
    def __init__(self, player1, player2):
        size = 8
        player1.num = 1
        player2.num = 2
        players = [player1, player2]
        pieces = {}
        for i in range(3):
            for j in range(0, 8, 2):
                if i == 0:
                    if j == 0:
                        pieces[i, j] = Bishop(player1)
                        pieces[size - i - 1, j + 1] = Camel(player2)
                    elif j == 6:
                        pieces[i, j] = Camel(player1)
                        pieces[size - i - 1, j + 1] = Bishop(player2)
                    else:
                        pieces[i, j] = CheckerKing(player1)
                        pieces[size - i - 1, j + 1] = CheckerKing(player2)
                elif i % 2:
                    pieces[i, j + 1] = Checker(player1, 'DOWN', [CheckerKing(player1), Camel(player1), Bishop(player1)])
                    pieces[size - i - 1, j] = Checker(player2, 'UP', [CheckerKing(player2), Camel(player2), Bishop(player2)])
                else:
                    pieces[i, j] = Checker(player1, 'DOWN', [CheckerKing(player1), Camel(player1), Bishop(player1)])
                    pieces[size - i - 1, j + 1] = Checker(player2, 'UP', [CheckerKing(player2), Camel(player2), Bishop(player2)])
        
        super().__init__(size, pieces, players)

    def lost(self, board, player_num):
        return len(board.moves(player_num)) == 0 or len([piece for piece in board.active_pieces(player_num) if piece.name == 'checker king']) == 0
