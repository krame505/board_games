
#include "MCTSPlayer.hpp"
#include "MCTS.hpp"

#include <iostream>

Move *MCTSPlayer::getMove(Game *game) const {
  GameTree *tree;
  if (leafParallel || treeParallel)
    tree = buildTree(game, trials, leafParallel, treeParallel, depth);
  else
    tree = buildTree(game, trials, depth);

  double maxScore = -1;
  Move *optMove = NULL;
  for (unsigned i = 0; i < tree->getChildren().size(); i++) {
    double score = tree->getChildren()[i]->getScores()[game->getTurn()];
    //cout << tree->getMoves()[i] << ": " << score << endl;
    if (score > maxScore) {
      optMove = tree->getMoves()[i];
      maxScore = score;
    }
  }

  Move *move = optMove->clone();
  delete tree;

  return move;
}
