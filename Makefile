FLAGS = -Wall -O2 -XOverloadedStrings
PACKAGE = -package parsec -package mtl

all: Roo

Roo: *.hs
	ghc Roo.hs -o Roo $(PACKAGE) $(FLAGS)

clean:
	rm -f *.o *.hi Roo
