
#include "HumanPlayer.hpp"

#include <string.h>

#include <iostream>
#include <vector>
using namespace std;

Move *HumanPlayer::getMove(Game *game) const {
  char input[100];

  cout << "Move for Player " << game->getTurn() << ": ";
  while (true) {
    cin.getline(input, 100);

    string error;
    vector<Move*> moves = game->parseMove(string(input), error);

    //for (Move *m : moves)
    //  cout << m << endl;

    if (moves.size() == 0) {
      if (error.size())
        cout << error << ", try again: ";
      else
        cout << "Invalid move, try again: ";
    }
    else if (moves.size() > 1) {
      cout << "Ambiguous move: ";
    }
    else {
      return moves[0];
    }
  }
}
