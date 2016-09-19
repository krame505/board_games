#pragma once

#include "Game.hpp"

#include <wchar.h>

#include <vector>
#include <stack>
#include <set>
#include <string>
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
    elems(new Piece*[width * height])
  {}

  RectBoard(const RectBoard &other) :
    width(other.width),
    height(other.height),
    elems(new Piece*[width * height]) {
    for (int i = 0; i < width * height; i++) {
      if (other.elems[i])
        elems[i] = other.elems[i]->clone();
    }
  }
  
  ~RectBoard() {
    delete elems;
  }

  // Get a pointer into the array corresponding to the start of row x
  // Then we can access as board[x][y] and do assignments
  Piece **operator[](int x) const {
    return &elems[x * width];
  }

  // Index on a loc directly
private: class LocIndexProxy;
public:
  LocIndexProxy operator[](loc l) {
    return LocIndexProxy(*this, l);
  }

  vector<Piece*> getPieces() const {
    vector<Piece*> result;
    for (int i = 0; i < height; i++) {
      for (int j = 0; j < width; j++) {
        if ((*this)[i][j])
          result.push_back((*this)[i][j]);
      }
    }
    return result;
  }

  vector<Piece*> getPieces(PlayerId owner) const {
    vector<Piece*> result;
    for (int i = 0; i < height; i++) {
      for (int j = 0; j < width; j++) {
        if ((*this)[i][j] && (*this)[i][j]->owner == owner)
          result.push_back((*this)[i][j]);
      }
    }
    return result;
  }
  
  // Fields
  const int width, height;
  stack<Piece*> removedElems;
private:
  Piece **elems;

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

    Piece *operator->() {
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
  
  // Execute the move on the given board, returning a set of created pieces
  virtual set<Piece*> doMove(RectBoard&) = 0;

  // Undo the move on the given board, returning a set of deleted pieces
  virtual set<Piece*> undoMove(RectBoard&) = 0;
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
    for (Piece *p : pieces)
      delete p;
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
    pieces.insert(p);
  }

  friend ostream &RectBoardMove::show(ostream&, Game*);

private:
  set<Piece*> pieces;

  ostream &write(ostream&) const;
  vector<Move*> genMoves();

  void doMove(Move *m) {
    set<Piece*> newPieces = ((RectBoardMove*)m)->doMove(board);
    pieces.insert(newPieces.begin(), newPieces.end());
  }

  void undoMove(Move *m) {
    set<Piece*> newPieces = ((RectBoardMove*)m)->doMove(board);
    pieces.erase(newPieces.begin(), newPieces.end());
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

  set<Piece*> doMove(RectBoard &board) {
    assert(board[l1] != NULL);
    board[l2] = board[l1];
    board[l1] = NULL;
    return set<Piece*>();
  }

  set<Piece*> undoMove(RectBoard &board) {
    assert(board[l2] != NULL);
    board[l1] = board[l2];
    board[l2] = NULL;
    return set<Piece*>();
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

  set<Piece*> doMove(RectBoard &board) {
    assert(board[l1] != NULL);
    assert(board[l2] != NULL);
    Piece *tmp = board[l1];
    board[l1] = board[l2];
    board[l2] = tmp;
    return set<Piece*>();
  }

  set<Piece*> undoMove(RectBoard &board) {
    assert(board[l1] != NULL);
    assert(board[l2] != NULL);
    Piece *tmp = board[l1];
    board[l1] = board[l2];
    board[l2] = tmp;
    return set<Piece*>();
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

  set<Piece*> doMove(RectBoard &board) {
    assert(board[l] != NULL);
    board.removedElems.push(board[l]);
    board[l] = NULL;
    return set<Piece*>();
  }

  set<Piece*> undoMove(RectBoard &board) {
    assert(board[l] == NULL);
    board[l] = board.removedElems.top();
    board.removedElems.pop();
    return set<Piece*>();
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

  set<Piece*> doMove(RectBoard &board) {
    assert(board[l] != NULL);
    Piece *p = piece->clone();
    board.removedElems.push(board[l]);
    board[l] = p;
    return set<Piece*> {p};
  }

  set<Piece*> undoMove(RectBoard &board) {
    assert(board[l] != NULL);
    Piece *p = board[l];
    delete p;
    board[l] = board.removedElems.top();
    board.removedElems.pop();
    return set<Piece*> {p};
  }

private:
  Piece *piece;
  loc l;
};

class SeqMove : public RectBoardMove {
public:
  SeqMove(vector<RectBoardMove*> moves) :
    moves(moves) {}

  SeqMove(const SeqMove &other) :
    moves(other.moves) {
    for (RectBoardMove *&move : moves)
      move = move->clone();
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

  set<Piece*> doMove(RectBoard &board) {
    set<Piece*> result;
    for (RectBoardMove *move : moves) {
      set<Piece*> res = move->doMove(board);
      result.insert(res.begin(), res.end());
    }
    return result;
  }

  set<Piece*> undoMove(RectBoard &board) {
    set<Piece*> result;
    for (vector<RectBoardMove*>::reverse_iterator it = moves.rbegin(); it != moves.rend(); it++) {
      RectBoardMove *move = *it;
      set<Piece*> res = move->undoMove(board);
      result.insert(res.begin(), res.end());
    }
    return result;
  }

private:
  vector<RectBoardMove*> moves;
};
