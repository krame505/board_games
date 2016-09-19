from move import *
from player import Player

from abc import ABCMeta, abstractmethod

class Piece:
    """Common abstract base class for pieces"""

    __metaclass__ = ABCMeta

    colors = ['green', 'cyan', 'magenta', 'blue', 'yellow']

    def __init__(self, name, label, player):
        self.name = name
        self.label = label
        self.color = None#Piece.colors[player_num - 1]
        self.player_num = player

    def __str__(self):
        return self.name

    # Allows sorting on tuples containing Pieces
    def __lt__(self, other):
        return self.name < other.name

    @abstractmethod
    def moves(self, x, y, board):
        pass
