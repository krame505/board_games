from board import *

from abc import ABCMeta, abstractmethod
from copy import copy

class Game:
    """Common abstract base class for games"""
    __metaclass__ = ABCMeta

    def __init__(self, size, num_players):
        self.size = size
        self.num_players = num_players
        self.board = Board(size, self, self.pieces())
    
    # Check if the game has been lost by a player
    @abstractmethod
    def lost(self, board, player_num):
        pass

    # Get the initial pieces
    @abstractmethod
    def pieces(self):
        pass

    # Main driver for playing games
    def play(self, players, verbose = False):
        # Initialize player numbers if not initialized already
        old_nums = [p.num for p in players]
        for i, p in enumerate(players):
            if p.num == None:
                p.num = i + 1
        players.sort(key = lambda p: p.num)

        board = self.board
        turn = 0
        num_players = len(players)

        # Play while there are player remaining
        while num_players > 1:
            player = players[turn]
            # If the player is still in the game
            if player.num != None:
                # Check if the player has lost
                if self.lost(board, player.num):
                    players[turn].num = None
                    num_players -= 1
                else:
                    # Get a move and update the board
                    if verbose:
                        print()
                        print(str(player) + "'s turn")
                        print(board)
                    move = player.get_move(board, verbose)
                    board = board.move(move)
            # Advance the turn
            turn = (turn + 1) % len(players)

        # Find the winner number
        winner = [p.num for p in players if p.num != None][0]
        if verbose:
            print("Player", winner, "won!")
            print(board)

        # Restore old player numbers
        for p, n in zip(players, old_nums):
            p.num = n

        return winner

        
