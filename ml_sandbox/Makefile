COMPILER=ghc
FLAGS=--make
TARGET=spl-parser
SRC=main.hs

$(TARGET): $(SRC)
	$(COMPILER) $(FLAGS) -o $@ $(SRC)

clean:
	rm -f *.hi
	rm -f *.o
	rm -f $(TARGET)
