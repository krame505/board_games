#!/usr/bin/python3
"""Genetic optimization script for heurstic player"""

from chesskers import *
from heuristicminmax import *

from multiprocessing import Process, Lock
import os
import time
import copy
import random
import json
from queue import LifoQueue

def mutate(scores):
    """Perform a mutation on a board state"""

    new_weights = copy.deepcopy(scores)
    entry = new_weights[random.choice(list(scores.keys()))]
    w = entry[0]
    l = entry[1]
    if random.choice([True, False]):
        i = random.randint(0, len(w) - 1)
        j = random.randint(0, len(w) - 1)
        if w[i][j] > 1:
            w[i][j] += random.choice([-1, 1])
        else:
            w[i][j] += 1
    elif random.choice([True, False, False, False, False]):
        l.insert(random.randint(0, len(l)), random.randint(1, 10))
    elif random.choice([True, False, False, False, False]) and len(l) > 1:
        del l[random.randint(0, len(l) - 1)]
    elif random.choice([True, False]):
        i = random.randint(0, len(l) - 1)
        if l[i] > 1:
            l[i] += random.choice([-1, 1])
        else:
            l[i] += 1
    
    return new_weights

# Play a match between players
def match(player1, player2):
    game = Chesskers()
    res1 = game.play([player1, player2])
    return res1
    # res2 = game.play([player2, player1])
    # if res1 == res2:
    #     return res1
    # else:
    #     res3 = game.play([player1, player2])
    #     return res3

# Process worker, performs a single genetic optimization (can't do more due to memory leakage)
def run_genetic(pool_file, lock):
    # Load the pool, or create it if empty
    lock.acquire()
    try:
        pool = json.loads(open(pool_file).read())
    except FileNotFoundError:
        pool = [HeuristicMinMaxSearchPlayer(weights='default').weights]
        open(pool_file, 'w').write(json.dumps(pool))
    lock.release()

    # Generate test weights
    test = mutate(random.choice(pool[:len(pool) // 2 + 1]))
    while test in pool:
        test = mutate(random.choice(pool[:len(pool) // 2 + 1]))
    player1 = HeuristicMinMaxSearchPlayer(2, test, 5)

    # "Bubble up" the test weights through the pool
    i = len(pool) - 1
    ref = pool[i]
    refs = LifoQueue()
    refs.put(ref)
    while i >= 0:
        #print("Playing vs.", i)
        player2 = HeuristicMinMaxSearchPlayer(2, ref, 5)
        if match(player2, player1) == 1:
            lock.acquire()
            pool = json.loads(open(pool_file).read())
            while ref not in pool and not refs.empty():
                ref = refs.get()
            if refs.empty():
                i = len(pool) - 1
            else:
                i = pool.index(ref) - 1
            ref = pool[i]
            refs.put(ref)
            lock.release()
        else:
            refs.put(ref)
            break

    # Save the updated pool to a file
    lock.acquire()
    pool = json.loads(open(pool_file).read())
    if ref in pool and i < (len(pool) // 2 + 1):
        i = pool.index(ref)
        if i <= len(pool) // 2:
            print("Adding to pool at level", i)
        if i == 0:
            print("Found new optimum")
        pool.insert(i, test)
        pool = pool[:7]
        open(pool_file, 'w').write(json.dumps(pool))
    lock.release()

# Create and start processes
if __name__ == '__main__':
    lock = Lock()
    procs = []
    while True:
        time.sleep(1)
        dead = []
        for p in procs:
            if not p.is_alive():
                dead.append(p)
        for p in dead:
            procs.remove(p)
        if len(procs) < 10:
            p = Process(target=run_genetic, args=('../data/scoring.json', lock))
            procs.append(p)
            p.start()

            
        

