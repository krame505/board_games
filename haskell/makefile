
GAMES=Nim Checkers
SCRIPTS=$(addprefix play_,$(GAMES))

SOURCES=$(wildcard *.hs)

COMPILE_FLAGS=-rtsopts -threaded -O3 # -debug
RUN_FLAGS=+RTS -N8

all: $(GAMES) $(SCRIPTS)

play_%: %
	echo "#!/bin/bash\n./$< \$$@ $(RUN_FLAGS)" > $@
	chmod +x $@

%: %.hs $(SOURCES)
	ghc $< $(COMPILE_FLAGS)

clean:
	rm -f $(GAMES) $(SCRIPTS) *.o *.hi *~
