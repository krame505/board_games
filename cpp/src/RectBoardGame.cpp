
#include "RectBoardGame.hpp"
#include "colors.h"

#include <string.h>
#include <iostream>
using namespace std;

ostream &operator<<(ostream &os, loc l) {
  return os << string(1, 'a' + l.y) << (l.x + 1);
}

ostream &RectBoardMove::show(ostream &os, Game *g) {
  return show(os, ((RectBoardGame*)g)->board);
}

vector<Piece*> RectBoard::getPieces() const {
  vector<Piece*> result;
  for (int i = 0; i < height; i++) {
    for (int j = 0; j < width; j++) {
      if ((*this)[i][j])
        result.push_back((*this)[i][j]);
    }
  }
  return result;
}

vector<Piece*> RectBoard::getPieces(PlayerId owner) const {
  vector<Piece*> result;
  for (int i = 0; i < height; i++) {
    for (int j = 0; j < width; j++) {
      if ((*this)[i][j] && (*this)[i][j]->owner == owner)
        result.push_back((*this)[i][j]);
    }
  }
  return result;
}

vector<Move*> RectBoardGame::genMoves() {
  vector<Move*> result;
  for (int i = 0; i < board.height; i++) {
    for (int j = 0; j < board.width; j++) {
      if (board[i][j] && board[i][j]->owner == getTurn()) {
        vector<RectBoardMove*> moves = board[i][j]->getMoves(board, loc(i, j));
        result.insert(result.end(), moves.begin(), moves.end());
      }
    }
  }
  return result;
}

ostream &RectBoardGame::write(ostream &os) const {
  for (int i = 0; i < board.width; i++) {
    os << "  " << string(1, 'a' + i);
  }
  os << "\n";
  for (int i = board.height - 1; i >= 0; i--) {
    os << (i + 1) << " ";
    for (int j = 0; j < board.width; j++) {
      os << EFFECT(BACKGROUND((i + j) % 2));
      if (board[i][j]) {
        if (board[i][j]->owner > 2)
          os << EFFECT(FOREGROUND(board[i][j]->owner / 2 + 1));
        os << board[i][j]->label << " ";
      }
      else {
        os << EFFECT(FOREGROUND(BLUE));
        os << string(1, 'a' + j) << (i + 1);
      }
      os << EFFECT(FOREGROUND(DEFAULT));
      os << EFFECT(BACKGROUND(DEFAULT));
      os << " ";
    }
    os << (i + 1) << "\n";
  }
  for (int i = 0; i < board.width; i++) {
    os << "  " << string(1, 'a' + i);
  }
  return os;
}
