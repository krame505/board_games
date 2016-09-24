
#include "MCTSPlayer.hpp"
#include "MCTS.hpp"

Move *MCTSPlayer::getMove(Game *game) const {
  GameTree *tree;
  if (leafParallel || treeParallel)
    tree = buildTree(game, trials, leafParallel, treeParallel);
  else
    tree = buildTree(game, trials);

  double maxScore = -1;
  Move *optMove = NULL;
  for (unsigned i = 0; i < tree->getChildren().size(); i++) {
    if (tree->getChildren()[i]->getScores()[game->getTurn()] > maxScore)
      optMove = tree->getMoves()[i];
  }

  Move *move = optMove->clone();
  delete tree;

  return move;
}
