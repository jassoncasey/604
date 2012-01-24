COMPILER=ghc
FLAGS=--make
TARGET=splc
SRC=lexer.hs

$(TARGET): $(SRC)
	$(COMPILER) $(FLAGS) -o $@ $(SRC)

clean:
	rm -f *.hi
	rm -f *.o
	rm $(TARGET)
