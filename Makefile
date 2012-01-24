COMPILER=ghc
FLAGS=--make
TARGET=test
SRC=test.hs

$(TARGET): $(SRC)
	$(COMPILER) $(FLAGS) -o $@ $(SRC)

clean:
	rm -f *.hi
	rm -f *.o
	rm $(TARGET)
