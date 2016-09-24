
#include "Game.hpp"

#include <vector>
#include <iostream>
using namespace std;

ostream &operator<<(ostream &os, Game *g) {
  return g->write(os);
}

ostream &operator<<(ostream &os, Move *m) {
  return m->write(os);
}

PlayerId play(Game *game, vector<Player*> players) {
  while (!game->isGameOver()) {
    cout << endl;
    cout << "Player " << game->getTurn() + 1 << "'s turn" << endl;
    cout << game << endl;
    Move *m = players[game->getTurn()]->getMove(game);
    cout << "Player " << game->getTurn() + 1 << " ";
    m->show(cout, game) << endl;
    game->move(m);
  }
  PlayerId winner = game->getWinner();
  if (winner < 0)
    cout << "Draw game!" << endl;
  else
    cout << "Player " << winner + 1 << " won!" << endl;
  cout << game << endl;
  return winner;
}
