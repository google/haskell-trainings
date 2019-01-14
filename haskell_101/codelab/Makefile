FLAGS   = -Wall -O2 -threaded -feager-blackholing
TARGETS = codelab solution



all: $(TARGETS)

codelab: Main.hs Codelab.hs
	ghc $(FLAGS) -DCODELAB $< -o $@

solution: Main.hs Solution.hs
	ghc $(FLAGS) -DSOLUTION $< -o $@

clean:
	rm -fv $(TARGETS) *.hi *.o
