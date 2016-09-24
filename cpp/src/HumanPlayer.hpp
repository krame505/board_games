#pragma once

#include "Game.hpp"

class HumanPlayer : public Player {
public:
  Move *getMove(Game *game) const;
};
