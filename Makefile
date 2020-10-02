all: Roo

Roo: *.hs
	ghc Roo.hs -o Roo -package parsec -package mtl

clean:
	rm -f *.o *.hi Roo
