#pragma once

#include "Game.hpp"

class GameTree {
public:
  GameTree(Game *game, GameTree *const parent):
    parent(parent),
    turn(game->getTurn()),
    trials(0),
    finished_trials(0),
    wins(game->numPlayers, 0) {
    for (Move *m : game->getMoves()) {
      moves.push_back(m->clone());
      children.push_back(NULL);
    }
  }

  ~GameTree() {
    for (Move *m : moves)
      delete m;
    for (GameTree *n : children)
      if (n != NULL)
        delete n;
  }

  vector<GameTree*> getChildren() const {
    return children;
  }

  vector<Move*> getMoves() const {
    return moves;
  }

  // Compute the fraction of wins for each player
  vector<double> getScores() const;

  // Compute the Upper Confidence Bound 1 scoring algorithm
  double ucb1() const;

  // Select a node for which to perform a playout and advance the game to that state
  GameTree *select(Game*);
  
  // Update the entire tree with the playout results
  void update(PlayerId);
  void update(vector<unsigned>, unsigned);
  
private:
  GameTree *parent;
  vector<GameTree*> children;
  vector<Move*> moves;
  PlayerId turn;

  unsigned long trials;
  unsigned long finished_trials;
  vector<unsigned> wins;
};

// Perform a random playout of a game
PlayerId playout(Game*);

// Perform n random playouts of a game
vector<unsigned> playouts(Game*, unsigned);

// Build a tree with the specified number of trials and (optional) degree of parallelism
GameTree *buildTree(Game*, unsigned long trials);
GameTree *buildTree(Game*, unsigned long trials, unsigned leafParallel, unsigned treeParallel);
