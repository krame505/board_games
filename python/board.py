from collections import namedtuple
from copy import copy
from termcolor import colored

# class Move(namedtuple('Move', 'fromX fromY toX toY captured promotion')):
#     def __hash__(self):
#         return hash((self.fromX, self.fromY, self.toX, self.toY))

class Board:
    """Data structure for storing a board state, non-mutable.  Holds pieces in a dict, also has a
    referenct to the game.  """

    def __init__(self, size, game, pieces = {}, prev_states = set()):
        self.size = size
        self.game = game
        self.pieces = pieces
        self.locations = {pieces[x, y]: (x, y) for x, y in pieces.keys()}
        self.prev_states = prev_states

    def __hash__(self):
        return hash(tuple((self.locations[p], p.name, p.player_num)
                          for p in self.locations.keys()))

    def __eq__(self, other):
        return (other != None and
                tuple((self.locations[p], p.name, p.player_num)
                     for p in self.locations.keys()) ==
                tuple((other.locations[p], p.name, p.player_num)
                     for p in other.locations.keys()))
    
    def __str__(self):
        return ("  " + "  ".join(chr(i + ord('a')) for i in range(self.size)) + "\n" + 
                "\n".join([str(self.size - i) + " " + 
                          " ".join([colored(self.pieces[i, j].label.ljust(2),
                                            color = self.pieces[i, j].color,
                                            on_color = 'on_red' if (i + j) % 2 else 'on_grey',
                                            attrs = ['bold'])
                                    if (i, j) in self.pieces
                                    else colored(chr(j + ord('a')) + str(self.size - i),
                                                 color = 'blue',
                                                 on_color = 'on_red' if (i + j) % 2 else 'on_grey')
                                    for j in range(self.size)]) +
                           " " + str(self.size - i)
                           for i in range(self.size)]) + "\n" + 
                "  " + "  ".join(chr(i + ord('a')) for i in range(self.size)))

    def __getitem__(self, index):
        return self.pieces[index] if index in self.pieces else None

    def __len__(self):
        return self.size
    
    def on_board(self, piece):
        return piece in self.locations

    def location(self, piece):
        return self.locations[piece]

    def is_open(self, x, y):
        return (x, y) not in self.pieces

    def active_pieces(self, player_num):
        return [piece
                for piece in self.pieces.values()
                if piece.player_num == player_num]

    def moves(self, player_num):
        return [move
                for piece in self.active_pieces(player_num)
                for move in piece.moves(self)
                if self.move(move) not in self.prev_states]

    def move(self, move):
        new_pieces = move.move(self.pieces)
        return Board(self.size, self.game, new_pieces, self.prev_states | {self})
