from piece import *

class Camel(Piece):
    """Implementation of camel piece, moves like chess knight, except 3x1 instead of 2x1 moves"""
    def __init__(self, player_num):
        name = 'camel'
        label = '♞' if player_num == 1 else '♘'
        super().__init__(name, label, player_num)

    def moves(self, board):
        if not board.on_board(self):
            return []

        result = []
        x, y = board.location(self)
        for shift1 in [-1, 1]:
            for shift2 in [-3, 3]:
                for shiftx, shifty in [(shift1, shift2), (shift2, shift1)]:
                    nx = x + shiftx
                    ny = y + shifty
                    if nx >= 0 and ny >= 0 and nx < board.size and ny < board.size:
                        if not board.is_open(nx, ny):
                            if board[nx, ny].player_num != self.player_num:
                                result.append(DirectMove(x, y, nx, ny, [(nx, ny)], None))
                        else:
                            result.append(DirectMove(x, y, nx, ny, [], None))

        return result
