UNAME = $(shell uname)

EXECUTABLES   = play

CPPC          = g++
CC            = gcc
CPPFLAGS      = -Wall -g -O0 -std=c++0x #-O3
CFLAGS 	      = -Wall -g -O0 #-O3

SOURCES += Game RectBoardGame Checkers
SOURCES += RandomPlayer
SOURCES += main

CPPFILES = $(addprefix ../src/,  $(SOURCES:=.cpp))
OBJFILES = $(addprefix ../bin/,  $(SOURCES:=.o))
EXEFILES = $(addprefix ../bin/,  $(EXECUTABLES))

#Libraries to include
LINK_LIBS += -lpthread

all: $(CPPFILES) $(OBJFILES) $(EXEFILES)

setup: ../bin

# Make sure bin/ exists
../bin:
	mkdir -p ../bin

clean:
	rm -f $(OBJFILES) $(EXEFILES) *~ ../bin/.fuse_hidden*

.PHONY: all setup clean

# places to look for included files
INCLUDE = 

# compile all objects
../bin/%.o: ../src/%.cpp ../src/%.hpp | ../bin
	$(CPPC) $< $(CPPFLAGS) $(INCLUDE) -c -o $@

# backup rule for objects without a header file (like main.o)
../bin/%.o: ../src/%.cpp | ../bin
	$(CPPC) $< $(CPPFLAGS) $(INCLUDE) -c -o $@

# rule for compiling c files with headers
../bin/%.o: ../src/%.c ../src/%.h | ../bin
	$(CC) $< $(CFLAGS) $(INCLUDE) -c -o $@

# rule for compiling c files without headers
../bin/%.o: ../src/%.c | ../bin
	$(CC) $< $(CFLAGS) $(INCLUDE) -c -o $@

# rule for compiling generated c files
../bin/%.o: ../bin/%.c | ../bin
	$(CC) $< $(CFLAGS) $(INCLUDE) -c -o $@

../bin/%: $(OBJFILES) 
	$(CPPC) $(OBJFILES) $(LINK_LIBS) -o $@
