COMPILER=ghc
FLAGS=--make
MAIN=spl-parser
INTERPRETER=spli
SRC=main.hs
SRCI=maini.hs

$(MAIN): $(SRC)
	$(COMPILER) $(FLAGS) -o $@ $(SRC)

all: $(MAIN) $(INTERPRETER)

$(INTERPRETER): $(SRCI)
	$(COMPILER) $(FLAGS) -o $@ $(SRCI)

tests: 

clean:
	rm -f *.hi
	rm -f *.o
	rm -f $(MAIN)
	rm -f $(INTERPRETER)
