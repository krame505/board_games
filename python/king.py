from piece import *

class King(Piece):
    """Implementation of chess king piece"""

    def __init__(self, player_num):
        name = 'king'
        label = '♚' if player_num == 1 else '♔'
        super().__init__(name, label, player_num)

    # TODO: Castling
    def moves(self, board):
        if not board.on_board(self):
            return []

        result = []
        x, y = board.location(self)
        for shiftx in range(-1, 2):
            for shifty in range(-1, 2):
                nx = x + shiftx
                ny = y + shifty
                if nx >= 0 and ny >= 0 and nx < board.size and ny < board.size:
                    if not board.is_open(nx, ny):
                        if board[nx, ny].player_num != self.player_num:
                            result.append(DirectMove(x, y, nx, ny, [(nx, ny)], None))
                    else:
                        result.append(DirectMove(x, y, nx, ny, [], None))

        return result
