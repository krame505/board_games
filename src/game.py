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
    
    @abstractmethod
    def lost(self, board, player_num):
        """Check if the game has been lost by a player"""
        pass

    @abstractmethod
    def pieces(self):
        """Get the initial pieces"""
        pass

    def play(self, players, board = None, verbose = False):
        """Main driver for playing games"""

        # Initialize player numbers if not initialized already
        old_players = [p for p in players]
        old_nums = [p.num for p in players]
        for i, p in enumerate(players):
            if p.num == None:
                p.num = i + 1
        players.sort(key = lambda p: p.num)

        if board == None:
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
                    if verbose and player.isAI:
                        print(move.show(board))
                    board = board.move(move)
            # Advance the turn
            turn = (turn + 1) % len(players)

        # Find the winner number
        winner = [p.num for p in players if p.num != None][0]
        if verbose:
            print("Player", winner, "won!")
            print(board)

        # Restore old player numbers
        for p, n in zip(old_players, old_nums):
            p.num = n

        return winner

        
