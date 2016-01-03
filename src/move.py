from abc import ABCMeta, abstractmethod

class Move:
    """Common abstract base class for moves"""
    __metaclass__ = ABCMeta

    @abstractmethod
    def __str__(self):
        return self.show(None)

    @abstractmethod
    def __repr__(self):
        pass

    @abstractmethod
    def __hash__(self):
        pass

    def __lt__(self, other):
        return True

    @abstractmethod
    def show(self, board):
        pass

    @abstractmethod
    def move(self, pieces):
        pass

class DirectMove(Move):
    def __init__(self, fromX, fromY, toX, toY, captured=[], promotion=None):
        self.fromX = fromX
        self.fromY = fromY
        self.toX = toX
        self.toY = toY
        self.captured = captured
        self.promotion = promotion
        
    def __repr__(self):
        return ("DirectMove(" +
                repr(self.fromX) + ", " +
                repr(self.fromY) + ", " +
                repr(self.toX) + ", " +
                repr(self.toY) + ", " +
                repr(self.captured) + ", " +
                repr(self.promotion) + ")")

    def __hash__(self):
        return hash((self.fromX, self.fromY, self.toX, self.toY))

    def show(self, board):
        return ("Moved " + (str(board[self.fromX, self.fromY]) + " at " if board else "") +
                chr(self.fromX + ord('A')) + str(self.fromY + 1) +
                " to " + chr(self.toX + ord('A')) + str(self.toY + 1) +
                "".join(", captured " + (str(board[i, j]) + " at " if board else "") +
                        chr(i + ord('A')) + str(j + 1) for i, j in self.captured) +
                (", promoted to " + str(self.promotion) if self.promotion else ""))

    def move(self, pieces):
        new_pieces = {(x, y): piece for (x, y), piece in pieces.items()
                      if (x, y) not in self.captured}
        new_pieces[self.toX, self.toY] = self.promotion if self.promotion else pieces[self.fromX, self.fromY]
        del new_pieces[self.fromX, self.fromY]
        return new_pieces

class NullMove(Move):
    def __repr__(self):
        return "NullMove"

    def __hash__(self):
        return 7

    def show(self, board):
        return "Moved nothing"

    def move(self, pieces):
        return pieces

class FnMove(Move):
    def __init__(self, fn, hashId=42):
        self.move = fn
        self.hashId = hashId

    def __repr__(self):
        return "FnMove(" + self.hashId + ")"
    
    def __hash__(self):
        return self.hashId
