
#include "MCTS.hpp"
#include "RandomPlayer.hpp"

#include <cmath>
#include <vector>
//#include <queue>
#include <cassert>
#include <iostream>
#include <thread>
#include <future>
#include <chrono>
using namespace std;

GameTree *GameTree::select(Game *game, int depth) {
  double optScore = -1;
  Move *optMove = NULL;
  GameTree *optNode = NULL;

  trials++;

  if (game->isGameOver())
    return this;

  for (unsigned i = 0; i < children.size(); i++) {
    if (children[i] == NULL) {
      game->move(moves[i]);
      children[i] = new GameTree(game, this);
      
      GameTree *tree = children[i];
      RandomPlayer p;
      while (depth > 0) {
        if (game->isGameOver())
          return tree;
        Move *m = p.getMove(game);
        game->move(m);
        tree = new GameTree(game, tree);
        depth--;
      }

      return tree;
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
  return optNode->select(game, depth - 1);
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
  for (unsigned w : wins) {
    result.push_back(w / (double)finished_trials);
  }
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
  for (int i = 0; i < 10000; i++) {
    if (game->isGameOver())
      return game->getWinner();
    Move *m = p.getMove(game);
    game->move(m);
  }
  return -1; // Assume a draw after 10000 moves
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

GameTree *buildTree(Game *game, unsigned long trials, int depth) {
  GameTree *tree = new GameTree(game, NULL);
  for (unsigned long i = 0; i < trials; i++) {
    //cout << "Trial " << i << endl;
    Game *cloneGame = game->clone();
    GameTree *leaf = tree->select(cloneGame, depth);
    PlayerId result = playout(cloneGame);
    leaf->update(result);
    delete cloneGame;
  }
  return tree;
}

GameTree *buildTree(Game *game, unsigned long trials, unsigned leafParallel, unsigned treeParallel, int depth) {
  GameTree *tree = new GameTree(game, NULL);
  vector<Game*> cloneGames;
  vector<GameTree*> leaves;
  vector<future<vector<unsigned>>> results;
  unsigned long i = 0;
  while (i < trials || results.size() > 0) {
    if (i < trials) {
      //cout << "Launching trial " << i << endl;
      Game *cloneGame = game->clone();
      cloneGames.push_back(cloneGame);
      leaves.push_back(tree->select(cloneGame, depth));
      results.push_back(async(launch::async, playouts, cloneGame, leafParallel));
      i++;
    }
    do {
      for (unsigned j = 0; j < results.size(); j++) {
        if (results[j].wait_for(chrono::seconds(0)) == future_status::ready) {
          //cout << "Joining " << j << endl;
          leaves[j]->update(results[j].get(), leafParallel);
          delete cloneGames[j];
          cloneGames.erase(cloneGames.begin() + j);
          leaves.erase(leaves.begin() + j);
          results.erase(results.begin() + j);
        }
      }
    } while (results.size() > treeParallel || (i >= trials && results.size() > 0));
  }
  return tree;
}
