COMPILER=ghc
FLAGS=-Wall
LIBS=-package haskell98 -package containers

# To add a new executable simple add the module name to TARGETS
TARGETS=spl test_lexer Driver

# To add a new source file simple append to SRC
SRC=Lexer.hs ParseTree.hs Parser.hs CmdOpts.hs PrettyPrint.hs Ast.hs \
	 Environment.hs Eval.hs Interactive.hs AstPrime.hs

all: $(TARGETS)

# Make all the programs from TESTS
$(TARGETS): $(SRC:%.hs=%.o) $(TARGETS:%=%.o)
	@echo "Building targets: $(TARGETS)"
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
