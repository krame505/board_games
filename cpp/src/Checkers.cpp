
#include "Checkers.hpp"

vector<RectBoardMove*> checkerMoves(RectBoard &board, loc l,
                                    vector<pair<int, int>> offsets,
                                    PlayerId owner,
                                    set<loc> promoLocs, vector<Piece*> promotions) {
  vector<RectBoardMove*> result =
    checkerDirectMoves(board, l, offsets, promoLocs, promotions);
  vector<RectBoardMove*> captureMoves =
    checkerCaptureMoves(board, l, offsets, owner, promoLocs, promotions);
  result.insert(result.end(), captureMoves.begin(), captureMoves.end());
  return result;
}

vector<RectBoardMove*> checkerDirectMoves(RectBoard &board, loc l,
                                          vector<pair<int, int>> offsets,
                                          set<loc> promoLocs, vector<Piece*> promotions) {
  vector<RectBoardMove*> result;
  for (pair<int, int> offset : offsets) {
    loc newLoc = loc(l.x + offset.first, l.y + offset.second);
    if (newLoc.x >= 0 && newLoc.y >= 0 &&
        newLoc.x < board.height && newLoc.y < board.width &&
        !board[newLoc]) {
      if (promoLocs.count(newLoc)) {
        for (Piece *piece : promotions)
          result.push_back(new SeqMove({
                new DirectMove(l, newLoc), new PromoteMove(piece->clone(), newLoc)}));
      }
      else
        result.push_back(new DirectMove(l, newLoc));
    }
  }
  return result;
}

vector<RectBoardMove*> checkerCaptureMoves(RectBoard &board, loc l,
                                           vector<pair<int, int>> offsets,
                                           PlayerId owner,
                                           set<loc> promoLocs, vector<Piece*> promotions) {
  vector<RectBoardMove*> result;
  for (pair<int, int> offset : offsets) {
    loc captureLoc = loc(l.x + offset.first, l.y + offset.second);
    loc newLoc = loc(l.x + 2 * offset.first, l.y + 2 * offset.second);
    if (newLoc.x >= 0 && newLoc.y >= 0 &&
        newLoc.x < board.height && newLoc.y < board.width &&
        board[captureLoc] && !board[newLoc] &&
        board[captureLoc]->owner != owner) {
      vector<RectBoardMove*> newMoves;
      if (promoLocs.find(newLoc) != promoLocs.end()) {
        for (Piece *piece : promotions)
          newMoves.push_back(new SeqMove({
                new DirectMove(l, newLoc),
                  new CaptureMove(captureLoc),
                  new PromoteMove(piece->clone(), newLoc)}));
      }
      else {
        newMoves = 
          {new SeqMove({new DirectMove(l, newLoc), new CaptureMove(captureLoc)})};
      }
      
      // Recursive capture moves
      for (RectBoardMove *newMove : newMoves) {
        result.push_back(newMove);
        newMove->doMove(board);
        vector<RectBoardMove*> moves =
          checkerCaptureMoves(board, newLoc, offsets, owner, promoLocs, promotions);
        newMove->undoMove(board);
        for (RectBoardMove *move : moves)
          result.push_back(new SeqMove(vector<RectBoardMove*> {newMove, move}));
      }
    }
  }
  
  return result;
}
