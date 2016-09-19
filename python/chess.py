from game import *
from king import *
from queen import *
from rook import *
from bishop import *
from knight import *
from pawn import *
import math

class ChessBoard(Board):
    """Special board for chess that restricts moves when a king is in check"""
    def moves(self, player_num):
        return [move
                for move in super().moves(player_num)
                if not self.move(move).in_check(player_num)]

    def move(self, move):
        new_pieces = move.move(self.pieces)
        return ChessBoard(self.size, self.game, new_pieces, self.prev_states | {self})

    def in_check(self, player_num):
        other_player = (player_num % 2) + 1
        for new_board in [self.move(move) for move in super().moves(other_player)]:
            if 'king' not in [piece.name for piece in new_board.active_pieces(player_num)]:
                print(new_board)
                return True
        return False

class Chess(Game):
    """Implementation of chess game"""

    def __init__(self):
        size = 8
        super().__init__(size, 2)

    def setup(self, size):
        """Get the initial board"""
        return ChessBoard(size, self, self.pieces())

    def pieces(self):
        pieces = {}
        for i in range(8):
            pieces[1, i] = Pawn(1, 'DOWN')
            pieces[6, i] = Pawn(2, 'UP')
        pieces[0, 0] = Rook(1)
        pieces[0, 7] = Rook(1)
        pieces[7, 0] = Rook(2)
        pieces[7, 7] = Rook(2)
        pieces[0, 1] = Knight(1)
        pieces[0, 6] = Knight(1)
        pieces[7, 1] = Knight(2)
        pieces[7, 6] = Knight(2)
        pieces[0, 2] = Bishop(1)
        pieces[0, 5] = Bishop(1)
        pieces[7, 2] = Bishop(2)
        pieces[7, 5] = Bishop(2)
        pieces[0, 3] = Queen(1)
        pieces[7, 3] = Queen(2)
        pieces[0, 4] = King(1)
        pieces[7, 4] = King(2)
        return pieces

    def action(self, board, player_num, verbose=False):
        if verbose and board.in_check(player_num):
            print("Player", player_num, "in check")

    def lost(self, board, player_num):
        player_moves = board.moves(player_num)
        if len(player_moves) == 0: # For simplicity, treating draw as a loss
            print("Draw")
            return True
        if not board.in_check(player_num):
            return False
        other_player = (player_num % 2) + 1
        for new_board1 in [board.move(move) for move in player_moves]:
            for new_board2 in [new_board1.move(move) for move in new_board1.moves(other_player)]:
                if 'king' in [piece.name for piece in new_board2.active_pieces(player_num)]:
                    break
            else:
                return True
        return False
