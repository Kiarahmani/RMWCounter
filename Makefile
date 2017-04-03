EXE=main
OPTS=-XCPP -rtsopts -threaded

all: $(EXE) 

main: Main.hs
	ghc $(OPTS) Main.hs

clean:
	rm -f *.dyn* *~ *.hi *.o $(EXE)
