
#include "MCTS.hpp"
#include "RandomPlayer.hpp"

#include <cmath>
#include <vector>
#include <queue>
#include <cassert>
#include <iostream>
#include <thread>
#include <future>
using namespace std;

GameTree *GameTree::select(Game *game) {
  double optScore = -1;
  Move *optMove = NULL;
  GameTree *optNode = NULL;

  trials++;

  for (unsigned i = 0; i < children.size(); i++) {
    if (children[i] == NULL) {
      game->move(moves[i]);
      children[i] = new GameTree(game, this);
      return children[i];
    }
    else {
      double score = children[i]->ucb1();
      if (score >= optScore) {
        optScore = score;
        optMove = moves[i];
        optNode = children[i];
      }
    }
  }
  
  game->move(optMove);
  return optNode->select(game);
}

void GameTree::update(PlayerId winner) {
  if (winner != -1)
    wins[winner]++;
  finished_trials++;
  if (parent)
    parent->update(winner);
}

void GameTree::update(vector<unsigned> newWins, unsigned numTrials) {
  for (unsigned i = 0; i < wins.size(); i++) {
    wins[i] += newWins[i];
  }
  finished_trials += numTrials;
  if (parent)
    parent->update(newWins, numTrials);
}

vector<double> GameTree::getScores() const {
  vector<double> result;
  for (unsigned w : wins)
    result.push_back(w / (double)finished_trials);
  return result;
}

double GameTree::ucb1() const {
  assert(parent != NULL);
  double result = sqrt(2.0L * log(parent->trials) / trials);
  if (finished_trials)
    result += wins[turn] / (double)finished_trials;
  // If there are no finished trials, then approximate the win ratio as all players winning equally
  else
    result += 1.0L / wins.size();
  return result;
}

PlayerId playout(Game *game) {
  RandomPlayer p;
  while (!game->isGameOver()) {
    Move *m = p.getMove(game);
    game->move(m);
  }
  PlayerId result = game->getWinner();
  return result;
}

vector<unsigned> playouts(Game *game, unsigned n) {
  vector<unsigned> wins(game->numPlayers, 0);
  for (unsigned i = 0; i < n; i++) {
    Game *newGame = game->clone();
    PlayerId winner = playout(newGame);
    if (winner != -1)
      wins[winner]++;
    delete newGame;
  }
  return wins;
}

GameTree *buildTree(Game *game, unsigned long trials) {
  GameTree *tree = new GameTree(game, NULL);
  for (unsigned long i = 0; i < trials; i++) {
    //cout << "Trial " << i << endl;
    Game *cloneGame = game->clone();
    GameTree *leaf = tree->select(cloneGame);
    PlayerId result = playout(cloneGame);
    leaf->update(result);
    delete cloneGame;
  }
  return tree;
}

GameTree *buildTree(Game *game, unsigned long trials, unsigned leafParallel, unsigned treeParallel) {
  GameTree *tree = new GameTree(game, NULL);
  queue<Game*> cloneGames;
  queue<GameTree*> leaves;
  queue<future<vector<unsigned>>> results;
  unsigned long i = 0;
  while (i < trials || results.size() > 0) {
    if (i < trials) {
      cout << "Launching trial " << i << endl;
      Game *cloneGame = game->clone();
      cloneGames.push(cloneGame);
      leaves.push(tree->select(cloneGame));
      results.push(async(launch::async, playouts, cloneGame, leafParallel));
      i++;
    }
    if (results.size() > treeParallel || i >= trials) {
      cout << "Joining" << endl;
      leaves.front()->update(results.front().get(), treeParallel);
      delete cloneGames.front();
      cloneGames.pop();
      leaves.pop();
      results.pop();
    }
  }
  return tree;
}
