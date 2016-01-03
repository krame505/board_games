from piece import *

class Checker(Piece):
    """Implementation of checker piece"""

    def __init__(self, player_num, direction, promotions = None):
        if promotions == None:
            promotions = [CheckerKing(player_num)]
        self.direction = direction
        self.promotions = promotions
        name = 'checker'
        label = '⛂' if player_num == 1 else '⛀'
        super().__init__(name, label, player_num)

    #move_cache = {}
    def moves(self, board):
        if not board.on_board(self):
            return []

        x, y = board.location(self)
        key = tuple(sorted(board.pieces.items())), x, y
        #if key in Checker.move_cache:
        #    return Checker.move_cache[key]
        

        result = []
        for shiftx in ([-1] if self.direction == 'UP' else
                       [1] if self.direction == 'DOWN' else
                       [-1, 1]):
            for shifty in ([-1] if self.direction == 'LEFT' else
                           [1] if self.direction == 'RIGHT' else
                           [-1, 1]):
                nx = x + shiftx
                ny = y + shifty
                if nx >= 0 and ny >= 0 and nx < board.size and ny < board.size and board.is_open(nx, ny):
                    result.append(DirectMove(x, y, nx, ny, [], None))
        def jump_moves(x, y):
            result = []
            for shiftx in ([-1] if self.direction == 'UP' else
                           [1] if self.direction == 'DOWN' else
                           [-1, 1]):
                for shifty in ([-1] if self.direction == 'LEFT' else
                               [1] if self.direction == 'RIGHT' else
                               [-1, 1]):
                    jx = x + shiftx
                    jy = y + shifty
                    nx = x + 2 * shiftx
                    ny = y + 2 * shifty
                    if (nx >= 0 and
                        ny >= 0 and
                        nx < board.size and
                        ny < board.size and
                        board[jx, jy] and
                        board[jx, jy].player_num != self.player_num and
                        board.is_open(nx, ny)):
                        result.append(DirectMove(x, y, nx, ny, [(jx, jy)], None))
                        for move in jump_moves(nx, ny):
                            result.append(DirectMove(x, y, move.toX, move.toY, [(jx, jy)] + move.captured, None))
            return result

        result.extend(jump_moves(x, y))

        to_remove = []
        new_moves = []
        for move in result:
            if ((move.toX == 0 and self.direction == 'UP') or
                (move.toX == board.size - 1 and self.direction == 'DOWN') or
                (move.toY == 0 and self.direction == 'LEFT') or
                (move.toY == board.size - 1 and self.direction == 'RIGHT')):
                to_remove.append(move)
                for promotion in self.promotions:
                    new_moves.append(DirectMove(move.fromX, move.fromY, move.toX, move.toY, move.captured, promotion))
        for move in to_remove:
            result.remove(move)
        result.extend(new_moves)

        #Checker.move_cache[key] = result
        return result

class CheckerKing(Piece):
    """Implementation of checker king piece"""

    def __init__(self, player_num):
        name = 'checker king'
        label = '⛃' if player_num == 1 else '⛁'
        super().__init__(name, label, player_num)

    def moves(self, board):
        if not board.on_board(self):
            return []

        result = []
        x, y = board.location(self)
        for shiftx in [-1, 1]:
            for shifty in [-1, 1]:
                nx = x + shiftx
                ny = y + shifty
                if nx >= 0 and ny >= 0 and nx < board.size and ny < board.size and board.is_open(nx, ny):
                    result.append(DirectMove(x, y, nx, ny, [], None))
        def jump_moves(x, y, visited):
            result = []
            for shiftx in [-1, 1]:
                for shifty in [-1, 1]:
                    jx = x + shiftx
                    jy = y + shifty
                    nx = x + 2 * shiftx
                    ny = y + 2 * shifty
                    if (nx >= 0 and
                        ny >= 0 and
                        nx < board.size and
                        ny < board.size and
                        board[jx, jy] and
                        board[jx, jy].player_num != self.player_num and
                        board.is_open(nx, ny) and
                        (nx, ny) not in visited):
                        result.append(DirectMove(x, y, nx, ny, [(jx, jy)], None))
                        for move in jump_moves(nx, ny, visited + [(nx, ny)]):
                            result.append(DirectMove(x, y, move.toX, move.toY, [(jx, jy)] + move.captured, None))
            return result

        result.extend(jump_moves(x, y, [(x, y)]))
        return result
