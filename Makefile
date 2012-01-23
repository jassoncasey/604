COMPILER=ghc
FLAGS=--make
TARGET=test
SRC=

$(TARGET):$(SRC)
	$(COMPILER) $(FLAGS) $@

clean:
	rm -f *.hi
	rm -f *.o
	rm $(TARGET)
