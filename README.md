# board_games
A python framework for chess/checkers-style board game AIs

## Python Installation
The termcolor library is used for displaying the board, it can be installed with
```
pip3 install termcolors
```
as root.  The sklearn library is also needed for the trained minmax AI, it can be installed using
```
apt-get install build-essential python3-dev python3-setuptools python3-numpy python3-scipy python3-pip libatlas-dev libatlas3gf-base
pip3 install sklearn
```
run as root.

## TODO items
* Finish monte carlo search player
* Add cutoff test to minmax
* Represent moves as functions
* Graphics (?)
* Implement Go
* Implement Chess
