#pragma once

#include "Game.hpp"

#include <wchar.h>

#include <vector>
#include <set>
#include <string>
#include <algorithm>
#include <iostream>
using namespace std;

struct loc {
  loc(int x, int y) : x(x), y(y) {}

  bool operator==(const loc &other) const {
    return x == other.x && y == other.y;
  }

  bool operator<(const loc &other) const {
    if (x == other.x)
      return y < other.y;
    else
      return x < other.x;
  }

  int x, y;
};

ostream &operator<<(ostream &os, loc l);

class RectBoardMove;
class RectBoard;

// Represents a piece on a RectBoard
class Piece {
public:
  Piece(string name, const char *label, PlayerId owner) :
    name(name),
    label(label),
    owner(owner)
  {}

  virtual ~Piece() {};

  virtual Piece *clone() const = 0;

  virtual vector<RectBoardMove*> getMoves(RectBoard &board, loc l) = 0;
  const string name;
  const char *label;
  const PlayerId owner;
};

enum Direction {NORTH, SOUTH, EAST, WEST};

// Data structure to store a rectangular board game
class RectBoard {
public:
  RectBoard(int width, int height) :
    width(width),
    height(height),
    pieces(new Piece*[width * height])
  {cout << width << ", " << height << endl;}

  RectBoard(const RectBoard &other) :
    width(other.width),
    height(other.height),
    pieces(new Piece*[width * height]) {
    for (int i = 0; i < width * height; i++) {
      if (other.pieces[i])
        pieces[i] = other.pieces[i]->clone();
    }
    for (Piece *p : other.hiddenPieces)
      hiddenPieces.push_back(p->clone());
  }
  
  ~RectBoard() {
    delete pieces;
  }

  // Delete the pieces contained in this board
  void deletePieces() {
    for (Piece *p : getPieces())
      delete p;
    for (Piece *p : hiddenPieces)
      delete p;
  }

  // Get a pointer into the array corresponding to the start of row x
  // Then we can access as board[x][y] and do assignments
  Piece **operator[](int x) const {
    return &pieces[x * width];
  }

  // Index on a loc directly
private: class LocIndexProxy;
public:
  LocIndexProxy operator[](loc l) {
    return LocIndexProxy(*this, l);
  }

  const Piece *operator[](loc l) const {
    return (*this)[l.x][l.y];
  }

  vector<Piece*> getPieces() const;
  vector<Piece*> getPieces(PlayerId owner) const;

  void hide(loc l) {
    assert((*this)[l] != NULL);
    hiddenPieces.push_back((*this)[l]);
    (*this)[l] = NULL;
  }

  void restore(loc l) {
    (*this)[l] = hiddenPieces.back();
    hiddenPieces.pop_back();
  }
  
  // Fields
  const int width, height;
private:
  Piece **pieces;
  vector<Piece*> hiddenPieces; // Stack of pieces currently not on the board

  // Hack needed for board[loc] = piece to work
  struct LocIndexProxy {
    LocIndexProxy(RectBoard &board, loc l) :
      board(board), l(l)
    {}

    Piece *operator=(Piece *piece) {
      return board[l.x][l.y] = piece;
    }

    Piece *operator=(LocIndexProxy &other) {
      return board[l.x][l.y] = (Piece *)other;
    }

    bool operator==(const Piece *piece) const {
      return board[l.x][l.y] == piece;
    }

    bool operator==(const LocIndexProxy &other) const {
      return board[l.x][l.y] == (Piece *)other;
    }
    
    Piece *operator->() {
      return board[l.x][l.y];
    }
    
    Piece *operator->() const {
      return board[l.x][l.y];
    }

    operator Piece*() const {
      return board[l.x][l.y];
    }
    
  private:
    RectBoard &board;
    loc l;
  };
};

// Move that is executed on a RectBoardGame
class RectBoardMove : public Move {
public:
  //RectBoardMove() {}
  //RectBoardMove(const RectBoardMove&);
  virtual ~RectBoardMove() {};

  virtual RectBoardMove *clone() = 0;

  ostream &show(ostream &os, Game *g);
  virtual ostream &show(ostream&, RectBoard&) = 0;
  
  // Execute the move on the given board
  virtual void doMove(RectBoard&) = 0;

  // Undo the move on the given board
  virtual void undoMove(RectBoard&) = 0;
};

// Base class for all kinds of games on a rectangular grid board
class RectBoardGame : public Game {
public:
  RectBoardGame(int numPlayers, int width, int height) :
    Game(numPlayers),
    board(width, height) {}

  RectBoardGame(const RectBoardGame &other) :
    Game(other),
    board(other.board) {}

  virtual ~RectBoardGame() {
    board.deletePieces();
  }

  // Public access to the board, but don't allow modification
  const RectBoard &getBoard() const {
    return board;
  }

protected:
  RectBoard board;

  vector<Piece*> getPieces() const {
    return board.getPieces();
  }

  vector<Piece*> getPieces(PlayerId owner) const {
    return board.getPieces(owner);
  }

  void addPiece(Piece *p, loc l) {
    board[l] = p;
  }

  friend ostream &RectBoardMove::show(ostream&, Game*);

private:
  ostream &write(ostream&) const;
  vector<Move*> parseMove(string, string&) const;
  vector<Move*> genMoves();

  void doMove(Move *m) {
    ((RectBoardMove*)m)->doMove(board);
  }

  void undoMove(Move *m) {
    ((RectBoardMove*)m)->undoMove(board);
  }
};

// Various types of RectBoardMoves
class DirectMove : public RectBoardMove {
public:
  DirectMove(loc l1, loc l2) :
    l1(l1), l2(l2) {}

  RectBoardMove *clone() {
    return new DirectMove(*this);
  }

  ostream &write(ostream &os) {
    return os << "moved " << l1 << " to " << l2;
  }

  ostream &show(ostream &os, RectBoard &board) {
    assert(board[l1] != NULL);
    return os << "moved " << board[l1]->name << " at " << l1 << " to " << l2;
  }

  void doMove(RectBoard &board) {
    assert(board[l1] != NULL);
    board[l2] = board[l1];
    board[l1] = NULL;
  }

  void undoMove(RectBoard &board) {
    assert(board[l2] != NULL);
    board[l1] = board[l2];
    board[l2] = NULL;
  }

private:
  loc l1, l2;
};

class SwapMove : public RectBoardMove {
public:
  SwapMove(loc l1, loc l2) :
    l1(l1), l2(l2) {}

  RectBoardMove *clone() {
    return new SwapMove(*this);
  }

  ostream &write(ostream &os) {
    return os << "swapped " << l1 << " with " << l2;
  }

  ostream &show(ostream &os, RectBoard &board) {
    assert(board[l1] != NULL);
    assert(board[l2] != NULL);
    return os << "swapped " << board[l1]->name << " at " << l1 <<
      " with " << board[l2]->name << " at " << l2;
  }

  void doMove(RectBoard &board) {
    assert(board[l1] != NULL);
    assert(board[l2] != NULL);
    Piece *tmp = board[l1];
    board[l1] = board[l2];
    board[l2] = tmp;
  }

  void undoMove(RectBoard &board) {
    assert(board[l1] != NULL);
    assert(board[l2] != NULL);
    Piece *tmp = board[l1];
    board[l1] = board[l2];
    board[l2] = tmp;
  }

private:
  loc l1, l2;
};

class CaptureMove : public RectBoardMove {
public:
  CaptureMove(loc l) :
    l(l) {}

  RectBoardMove *clone() {
    return new CaptureMove(*this);
  }

  ostream &write(ostream &os) {
    return os << "captured " << l;
  }

  ostream &show(ostream &os, RectBoard &board) {
    assert(board[l] != NULL);
    return os << "captured " << board[l]->name << " at " << l;
  }

  void doMove(RectBoard &board) {
    assert(board[l] != NULL);
    board.hide(l);
  }

  void undoMove(RectBoard &board) {
    assert(board[l] == NULL);
    board.restore(l);
  }

private:
  loc l;
};

class PromoteMove : public RectBoardMove {
public:
  PromoteMove(Piece *piece, loc l) :
    piece(piece), l(l) {}

  PromoteMove(const PromoteMove &other) :
    PromoteMove(other.piece->clone(), other.l) {}

  ~PromoteMove() {
    delete piece;
  }

  RectBoardMove *clone() {
    return new PromoteMove(*this);
  }

  ostream &write(ostream &os) {
    return os << "promoted " << l << " to " << piece->name;
  }

  ostream &show(ostream &os, RectBoard &board) {
    assert(board[l] != NULL);
    return os << "promoted " << board[l]->name << " at " << l << " to " << piece->name;
  }

  void doMove(RectBoard &board) {
    assert(board[l] != NULL);
    Piece *p = piece->clone();
    board.hide(l);
    board[l] = p;
  }

  void undoMove(RectBoard &board) {
    assert(board[l] != NULL);
    Piece *p = board[l];
    delete p;
    board.restore(l);
  }

private:
  Piece *piece;
  loc l;
};

class SeqMove : public RectBoardMove {
public:
  SeqMove(vector<RectBoardMove*> moves) :
    moves(moves) {}

  SeqMove(const SeqMove &other) {
    for (RectBoardMove *move : other.moves)
      moves.push_back(move->clone());
  }

  ~SeqMove() {
    for (RectBoardMove *move : moves)
      delete move;
  }

  RectBoardMove *clone() {
    return new SeqMove(*this);
  }

  ostream &write(ostream &os) {
    for (unsigned i = 0; i < moves.size(); i++) {
      if (i > 0)
        os << ", ";
      moves[i]->write(os);
    }
    return os;
  }

  ostream &show(ostream &os, RectBoard &board) {
    for (unsigned i = 0; i < moves.size(); i++) {
      if (i > 0)
        os << ", ";
      moves[i]->show(os, board);
      moves[i]->doMove(board);
    }
    for (vector<RectBoardMove*>::reverse_iterator it = moves.rbegin(); it != moves.rend(); it++) {
      (*it)->undoMove(board);
    }
    return os;
  }

  void doMove(RectBoard &board) {
    for (RectBoardMove *move : moves) {
      move->doMove(board);
    }
  }

  void undoMove(RectBoard &board) {
    for (vector<RectBoardMove*>::reverse_iterator it = moves.rbegin(); it != moves.rend(); it++) {
      (*it)->undoMove(board);
    }
  }

private:
  vector<RectBoardMove*> moves;
};
