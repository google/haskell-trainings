FLAGS   = -Wall -O2 -threaded -feager-blackholing
TARGETS = codelab solution test_codelab test_solution



all: $(TARGETS)

test_codelab: Tests.hs Game.hs Codelab.hs
	ghc $(FLAGS) -DCODELAB $< -o $@

test_solution: Tests.hs Game.hs Solution.hs
	ghc $(FLAGS) -DSOLUTION $< -o $@

codelab: Main.hs Game.hs Codelab.hs
	ghc $(FLAGS) -DCODELAB $< -o $@

solution: Main.hs Game.hs Solution.hs
	ghc $(FLAGS) -DSOLUTION $< -o $@



clean:
	rm -fv $(TARGETS) *.hi *.o
