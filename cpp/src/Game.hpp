#pragma once

#include <vector>
//#include <stack>
#include <string>
#include <cassert>
using namespace std;

class Game;

class Move {
public:
  virtual ~Move() {};

  // Clone the move
  virtual Move *clone() = 0;

  // Print a game-independant representation of the move to an ostream
  virtual ostream &write(ostream&) = 0;

  // Print a game-dependant representation of the move to an ostream
  // WARNING: If you call this on a Game where this isn't a valid move
  // very bad things *will* happen.  
  virtual ostream &show(ostream&, Game*) = 0;
};

ostream &operator<<(ostream&, Move*);

typedef int PlayerId;

class Game {
public:
  Game(int numPlayers) :
    numPlayers(numPlayers),
    turn(0) {}

  Game(const Game &other) :
    numPlayers(other.numPlayers),
    turn(other.turn),
    moves(other.moves),
    prevMoves(other.prevMoves) {
    for (vector<Move*> ms : moves)
      for (Move *&m : ms)
        m = m->clone();
    for (Move *&m : prevMoves)
      m = m->clone();
  }

  virtual ~Game() {};

  virtual Game *clone() const = 0;

  PlayerId getTurn() const {
    return turn;
  }

  // Get the list of possible moves in the current state
  vector<Move*> getMoves() const {
    assert(moves.size() > 0);
    return moves.back();
  }

  // Make a move
  void move(Move *m) {
    assert(!isGameOver());
    
    // Need to clone m1 since we don't know when it might get deleted
    Move *m1 = m->clone();
    prevMoves.push_back(m1);
    doMove(m1);
    
    // Increment turn and rollover at numPlayers
    do {
      turn++;
      turn %= numPlayers;
    } while (!isPlayerActive(turn));

    // Generate the new set of moves
    moves.push_back(genMoves());
  }

  // Undo the last move
  void backtrack() {
    undoMove(prevMoves.back());
    delete prevMoves.back();
    prevMoves.pop_back();
    
    // Decrement turn and roll over at 0
    do {
      turn += numPlayers - 1;
      turn %= numPlayers;
    } while (!isPlayerActive(turn));

    // Restore the previous set of moves
    for (Move *m: moves.back())
      delete m;
    moves.pop_back();
  }

  friend ostream &operator<<(ostream&, Game*);

  // Generate the list of possible moves
  virtual vector<Move*> parseMove(string, string&) const = 0;

  // Return true if player i is active (i.e. shouldn't be skipped)
  virtual bool isPlayerActive(int) {
    return true;
  }

  // Return true if the game is won or drawn
  virtual bool isGameOver() const = 0;

  // Return winner, or -1 if a draw or not won
  virtual PlayerId getWinner() const = 0;

  const int numPlayers;

protected:
  void init() {
    // Initialize the first set of moves if this is the first turn
    if (moves.size() == 0)
      moves.push_back(genMoves());
  }

  // Display the board
  virtual ostream &write(ostream&) const = 0;

  // Generate the list of possible moves
  virtual vector<Move*> genMoves() = 0;

  // Execute the given move on the internal state
  virtual void doMove(Move*) = 0;

  // Undo the given move on the internal state
  virtual void undoMove(Move*) = 0;

private:
  PlayerId turn;
  
  // These are really stacks, but we need to iterate over them when cloning
  vector<vector<Move*>> moves;
  vector<Move*> prevMoves;
};

ostream &operator<<(ostream&, Game*);

class Player {
public:
  virtual ~Player() {};

  virtual Move *getMove(Game*) const = 0;
};

int play(Game *game, vector<Player*> players);
