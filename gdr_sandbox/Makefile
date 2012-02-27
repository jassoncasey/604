COMPILER=ghc
FLAGS=-Wall
LIBS=-package haskell98 -package containers

TARGETS=spl

SRC=Lexing.hs Parsing.hs Ast.hs Evaluate.hs Typing.hs ProofTree.hs

all: $(TARGETS)

$(TARGETS): $(SRC:%.hs=%.o) $(TARGETS:%=%.o)
	@echo "Building target: $(TARGETS)"
	@for test in $(TARGETS); do \
		echo $(COMPILER) $(FLAGS) $(LIBS) -o $$test $(SRC:%.hs=%.o) $$test.o; \
		$(COMPILER) $(FLAGS) $(LIBS) -o $$test $(SRC:%.hs=%.o) $$test.o; \
		done

%.o:%.hs
	$(COMPILER) $(FLAGS) -c $^

clean:
	rm -f *.hi
	rm -f *.o
	rm -f $(TARGETS)
