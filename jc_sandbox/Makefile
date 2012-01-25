COMPILER=ghc
FLAGS=--make
TARGET=splc
SRC=Lexer.hs Parser.hs Driver.hs

$(TARGET): $(SRC)
	$(COMPILER) $(FLAGS) -o $@ $(SRC)

clean:
	rm -f *.hi
	rm -f *.o
	rm -f $(TARGET)
