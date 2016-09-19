from piece import *

class Rook(Piece):
    """Implementation of chess rook piece"""

    def __init__(self, player_num):
        name = 'rook'
        label = '♜' if player_num == 1 else '♖'
        super().__init__(name, label, player_num)

    def moves(self, board):
        if not board.on_board(self):
            return []

        result = []
        x, y = board.location(self)
        for shiftx, shifty in [(-1, 0), (1, 0), (0, -1), (0, 1)]:
            for i in range(1, board.size):
                nx = x + i * shiftx
                ny = y + i * shifty
                if nx >= 0 and ny >= 0 and nx < board.size and ny < board.size:
                    if not board.is_open(nx, ny):
                        if board[nx, ny].player_num != self.player_num:
                            result.append(DirectMove(x, y, nx, ny, [(nx, ny)], None))
                        break
                    else:
                        result.append(DirectMove(x, y, nx, ny, [], None))

        return result
