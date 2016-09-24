#include "Game.hpp"

#include "Checkers.hpp"

#include "RandomPlayer.hpp"
#include "HumanPlayer.hpp"
#include "MCTSPlayer.hpp"

#include <string.h>
#include <iostream>
#include <vector>
using namespace std;

Game *getGame(string name) {
  if (name == "checkers")
    return new Checkers();
  else {
    cerr << "Invalid game " << name << endl;
    exit(1);
  }
  return NULL;
}

Player *getPlayer(string name) {
  if (name == "random")
    return new RandomPlayer();
  if (name == "human")
    return new HumanPlayer();
  if (name == "mcts")
    return new MCTSPlayer(1000, 10, 50);
  else {
    cerr << "Invalid player " << name << endl;
    exit(1);
  }
  return NULL;
}

int main(int argc, char *argv[]) {
  if (argc < 2) {
    cerr << "Expected game name" << endl;
    exit(1);
  }

  Game *game = getGame(string(argv[1]));

  vector<Player*> players;
  for (int i = 0; i < game->numPlayers; i++) {
    if (i + 2 < argc) {
      players.push_back(getPlayer(string(argv[i + 2])));
    }
    else {
      players.push_back(new HumanPlayer());
    }
  }

  // Initialize random seed
  srand (time(NULL));

  play(game, players);

  delete game;
  for (Player *player : players)
    delete player;
}
