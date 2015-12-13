from abc import ABCMeta, abstractmethod

class Player:
    """Common abstract base class for players"""
    __metaclass__ = ABCMeta

    def __init__(self, num = None):
        self.num = num

    def __str__(self):
        return "Player " + str(self.num)

    @abstractmethod
    def get_move(self, board, verbose = False):
        pass
