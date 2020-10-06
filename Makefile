FLAGS = -Wall -O2 -XOverloadedStrings -XMultiParamTypeClasses -XFlexibleInstances -XFlexibleContexts -fno-warn-name-shadowing -fno-warn-unused-do-bind
PACKAGE = -package parsec -package mtl

all: Roo

Roo: *.hs
	ghc Roo.hs -o Roo $(PACKAGE) $(FLAGS)

clean:
	rm -f *.o *.hi Roo
