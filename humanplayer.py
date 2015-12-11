from player import *

import sys

class HumanPlayer(Player):
    def get_move(self, board, verbose = False):
        moves = board.moves(self.num)
        prompt = "Move for " + str(self) + ": "
        while True:
            move = input(prompt).upper().split(' ')
            if len(move) >= 3:
                move[2] = ' '.join(move[2:]).lower()
            if (len(move) == 0 or
                (len(move) > 0 and (len(move[0]) != 2 or
                                    not move[0][0].isalpha() or
                                    not move[0][1].isdigit())) or
                (len(move) > 1 and (len(move[1]) != 2 or
                                    not move[1][0].isalpha() or
                                    not move[1][1].isdigit())) or
                (len(move) > 2 and not ''.join(move[2].split(' ')).isalpha())):
                prompt = "Syntax error, try again: "
            elif len(move) == 1:
                x = ord(move[0][0]) - ord('A')
                y = int(move[0][1]) - 1
                cantidates = [m for m in moves if m.toX == x and m.toY == y]
                if len(cantidates) == 1:
                    return cantidates[0]
                elif len(cantidates) > 1:
                    prompt = "Ambiguous move, try again: "
                else:
                    prompt = "Invalid move, try again: "
            elif len(move) == 2:
                fromX = ord(move[0][0]) - ord('A')
                fromY = int(move[0][1]) - 1
                toX = ord(move[1][0]) - ord('A')
                toY = int(move[1][1]) - 1
                cantidates = [m for m in moves 
                              if m.fromX == fromX and m.fromY == fromY and m.toX == toX and m.toY == toY]
                if len(cantidates) == 1:
                    return cantidates[0]
                elif len(cantidates) > 1:
                    prompt = "Ambiguous move, try again: "
                else:
                    prompt = "Invalid move, try again: "
            elif len(move) >= 3:
                fromX = ord(move[0][0]) - ord('A')
                fromY = int(move[0][1]) - 1
                toX = ord(move[1][0]) - ord('A')
                toY = int(move[1][1]) - 1
                promotion = move[2]
                cantidates = [m for m in moves 
                              if m.fromX == fromX
                              and m.fromY == fromY
                              and m.toX == toX
                              and m.toY == toY
                              and m.promotion != None
                              and m.promotion.name == promotion]
                if len(cantidates) == 1:
                    return cantidates[0]
                elif len(cantidates) > 1:
                    sys.exit("Fatal error: multiple identical moves in cantidates")
                else:
                    prompt = "Invalid move, try again: "
