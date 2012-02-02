COMPILER=ghc
FLAGS=-package haskell98 -Wall

# To add a new executable simple add the module name to TARGETS
TARGETS=spl test_lexer test_parser

# To add a new source file simple append to SRC
SRC=Lexer.hs Ast.hs Parser.hs CmdOpts.hs 

all: $(TARGETS)

# Make all the programs from TESTS
$(TARGETS): $(SRC:%.hs=%.o) $(TARGETS:%=%.o)
	@echo "Building targets: $(TARGETS)"
	@for test in $(TARGETS); do \
		echo $(COMPILER) $(FLAGS) -o $$test $(SRC:%.hs=%.o) $$test.o; \
		$(COMPILER) $(FLAGS) -o $$test $(SRC:%.hs=%.o) $$test.o; \
		done

%.o:%.hs
	$(COMPILER) $(FLAGS) -c $^

clean:
	rm -f *.hi
	rm -f *.o
	rm -f $(TARGETS)
