#pragma once

#include "RectBoardGame.hpp"

#include <vector>
#include <set>
#include <utility>
using namespace std;

vector<RectBoardMove*> checkerMoves(RectBoard&, loc, vector<pair<int, int>>, PlayerId, set<loc>, vector<Piece*>);
vector<RectBoardMove*> checkerDirectMoves(RectBoard&, loc, vector<pair<int, int>>, set<loc>, vector<Piece*>);
vector<RectBoardMove*> checkerCaptureMoves(RectBoard&, loc, vector<pair<int, int>>, PlayerId, set<loc>, vector<Piece*>);

class CheckerKing : public Piece {
public:
  CheckerKing(PlayerId owner) :
    Piece("checker king", owner % 2? "⛃" : "⛁", owner) {}

  Piece *clone() const {
    return new CheckerKing(*this);
  }

  vector<RectBoardMove*> getMoves(RectBoard &board, loc l) {
    vector<pair<int, int>> offsets = {{1, 1}, {1, -1}, {-1, 1}, {-1, -1}};
    return checkerMoves(board, l, offsets, owner, {}, {});
  }
};

class Checker : public Piece {
public:
  Checker(PlayerId owner, Direction d) :
    Piece("checker", owner % 2? "⛂" : "⛀", owner),
    d(d) {}

  Piece *clone() const {
    return new Checker(*this);
  }

  vector<RectBoardMove*> getMoves(RectBoard &board, loc l) {
    vector<pair<int, int>> offsets;
    set<loc> promoLocs;
    vector<Piece*> promotions = {new CheckerKing(owner)};
    switch (d) {
    case NORTH:
      offsets = vector<pair<int, int>> {{1, -1}, {1, 1}};
      for (int i = 1; i < board.width; i+=2)
        promoLocs.emplace(loc(board.height - 1, i));
      break;
    case SOUTH:
      offsets = vector<pair<int, int>> {{-1, -1}, {-1, 1}};
      for (int i = 0; i < board.width; i+=2)
        promoLocs.emplace(loc(0, i));
      break;
    case EAST:
      offsets = vector<pair<int, int>> {{1, 1}, {-1, 1}};
      for (int i = 1; i < board.height; i+=2)
        promoLocs.emplace(loc(i, board.width - 1));
      break;
    case WEST:
      offsets = vector<pair<int, int>> {{1, -1}, {-1, -1}};
      for (int i = 0; i < board.height; i+=2)
        promoLocs.emplace(loc(i, 0));
      break;
    }
    vector<RectBoardMove*> result =
      checkerMoves(board, l, offsets, owner, promoLocs, promotions);
    for (Piece *piece : promotions)
      delete piece;
    return result;
  }

private:
  Direction d;
};

class Checkers : public RectBoardGame {
public:
  Checkers() :
    RectBoardGame(2, 8, 8) {
    for (int i = 0; i < 4; i++) {
      for (int j = 0; j < 3; j++) {
        addPiece(new Checker(0, SOUTH), loc(7 - j, i * 2 + (j + 1) % 2));
        addPiece(new Checker(1, NORTH), loc(j, i * 2 + j % 2));
      }
    }

    init();
  }

  Game *clone() const {
    return new Checkers(*this);
  }

  bool isGameOver() const {
    if (getMoves().size() == 0)
      return true;

    for (int player = 0; player < numPlayers; player++) {
      if (board.getPieces(player).size() == 0) {
        return true;
      }
    }
    
    return false;
  }

  int getWinner() const {
    if (getMoves().size() == 0)
      return 1 - getTurn();

    for (int player = 0; player < numPlayers; player++) {
      if (board.getPieces(player).size() == 0) {
        return 1 - player;
      }
    }
    
    return -1;
  }
};
