#pragma once

#include "Game.hpp"

using namespace std;

class MCTSPlayer : public Player {
public:
  MCTSPlayer(unsigned long trials, unsigned leafParallel=0, unsigned treeParallel=0, int depth=0) :
    trials(trials),
    leafParallel(leafParallel),
    treeParallel(treeParallel),
    depth(depth)
  {}

  Move *getMove(Game*) const;

private:
  unsigned long trials;
  unsigned leafParallel;
  unsigned treeParallel;
  int depth;
};
