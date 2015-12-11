# Common superclass for games
from board import *

from abc import ABCMeta, abstractmethod
from copy import copy

class Game:
    __metaclass__ = ABCMeta

    def __init__(self, size, pieces, players):
        self.size = size
        self.pieces = pieces
        self.players = players
        self.board = Board(size, self, pieces)
    
    # Check if the game has been lost by a player
    @abstractmethod
    def lost(self, board, player_num):
        pass

    # Main driver for playing games
    def play(self, verbose = False):
        board = self.board
        players = copy(self.players)
        turn = 0
        num_players = len(players)

        # Play while there are player remaining
        while num_players > 1:
            player = players[turn]
            # If the player is still in the game
            if player:
                # Check if the player has lost
                if self.lost(board, player.num):
                    players[turn] = None
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
        if verbose:
            print("Player", [p for p in players if p][0].num, "won!")
            print(board)
        return [p for p in players if p][0].num

        
