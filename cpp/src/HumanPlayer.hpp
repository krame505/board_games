#pragma once

#include "Game.hpp"
#include "RectBoardGame.hpp"

class HumanPlayer : public Player {
public:
  Move *getMove(Game *game) const;
};
