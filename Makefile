EXE= server client broker create drop
OPTS=-XCPP -rtsopts -threaded

all: $(EXE) 

main: Main.hs
	ghc $(OPTS) Main.hs

client: ClientDeamon.hs
	ghc $(OPTS) ClientDeamon.hs

create: CreateDeamon.hs
	ghc $(OPTS) CreateDeamon.hs

drop: DropDeamon.hs
	ghc $(OPTS) DropDeamon.hs


broker: BrokerDeamon.hs 
	ghc $(OPTS) BrokerDeamon.hs

server: ServerDeamon.hs
	ghc $(OPTS) ServerDeamon.hs

clean:
	rm -f *.dyn* *~ *.hi *.o $(EXE)
