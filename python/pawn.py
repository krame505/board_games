from piece import *

from rook import Rook
from bishop import Bishop
from knight import Knight
from queen import Queen

class Pawn(Piece):
    """Implementation of chess pawn"""

    def __init__(self, player_num, direction, promotions = None):
        if promotions == None:
            promotions = [Rook(player_num), Knight(player_num), Bishop(player_num), Queen(player_num)]
        self.direction = direction
        self.promotions = promotions
        name = 'pawn'
        label = '♟' if player_num == 1 else '♙'
        super().__init__(name, label, player_num)

    # TODO: Enpassant
    def moves(self, board):
        if not board.on_board(self):
            return []

        x, y = board.location(self)

        result = []
        shiftx = 0
        shifty = 0
        if self.direction == 'UP':
            shiftx = -1
        if self.direction == 'DOWN':
            shiftx = 1
        if self.direction == 'LEFT':
            shifty = -1
        if self.direction == 'RIGHT':
            shifty = 1
        for capture_shift in range(-1, 2):
            nx = x + shiftx
            ny = y + shifty
            if self.direction == 'UP' or self.direction == 'DOWN':
                nx += capture_shift
            elif self.direction == 'LEFT' or self.direction == 'RIGHT':
                ny += capture_shift
            if (board.is_open(nx, ny) and capture_shift == 0 or
                (not board.is_open(nx, ny) and capture_shift != 0 and
                 board[nx, ny].player_num != self.player_num)):
                captures = [] if capture_shift == 0 else [(nx, ny)]
                if (((self.direction == 'UP' and nx == 0) or
                     (self.direction == 'LEFT' and ny == 0) or
                     (self.direction == 'DOWN' and nx == board.size - 1) or
                     (self.direction == 'RIGHT' and ny == board.size - 1)) and
                    (board.is_open(nx, ny) or capture_shift != 0)):
                    result.extend(DirectMove(x, y, nx, ny, captures, p) for p in self.promotions)
                elif (nx >= 0 and ny >= 0 and
                      nx < board.size and ny < board.size and
                      (board.is_open(nx, ny) or capture_shift != 0)):
                    result.append(DirectMove(x, y, nx, ny, captures, None))
        if ((self.direction == 'UP' and x == 6) or
            (self.direction == 'DOWN' and x == 1) or
            (self.direction == 'LEFT' and x == 6) or
            (self.direction == 'RIGHT' and x == 1)):
            nx = x + shiftx * 2
            ny = y + shifty * 2
            if board.is_open(nx, ny):
                result.append(DirectMove(x, y, nx, ny, [], None))

        return result
