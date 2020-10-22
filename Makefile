FLAGS = -Wall -g -fno-warn-name-shadowing -fno-warn-unused-do-bind -fbreak-on-exception 
PACKAGE = -package parsec -package mtl

all: Roo

Roo: Common.hs Oz.hs Roo.hs RooAnalyse.hs RooAst.hs RooCompile.hs RooParser.hs RooPrettyPrinter.hs SymTable.hs
	ghc Roo.hs -o Roo $(PACKAGE) $(FLAGS)

clean:
	rm -f *.o *.hi Roo
