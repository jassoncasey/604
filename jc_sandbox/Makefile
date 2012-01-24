COMPILER=ghc
FLAGS=--make
TARGET=splc
SRC=lexer.hs parser.hs driver.hs

$(TARGET): $(SRC)
	$(COMPILER) $(FLAGS) -o $@ $(SRC)

clean:
	rm -f *.hi
	rm -f *.o
	rm $(TARGET)
