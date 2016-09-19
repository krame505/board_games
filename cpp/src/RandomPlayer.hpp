#pragma once

#include "Game.hpp"

using namespace std;

class RandomPlayer : public Player {
public:
  Move *getMove(Game*) const;
};
