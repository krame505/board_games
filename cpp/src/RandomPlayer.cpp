
#include "RandomPlayer.hpp"

#include <random>

Move *RandomPlayer::getMove(Game *game) const {
  vector<Move*> moves = game->getMoves();
  return moves[rand() % moves.size()];
}
