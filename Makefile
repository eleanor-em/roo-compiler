FLAGS = -Wall -O2 -fno-warn-name-shadowing -fno-warn-unused-do-bind
PACKAGE = -package parsec -package mtl

all: Roo

Roo: Common.hs Oz.hs Roo.hs RooAnalyse.hs RooAst.hs RooCompile.hs RooParser.hs RooPrettyPrinter.hs SymTable.hs RooPreprocessor.hs
	ghc Roo.hs -o Roo $(PACKAGE) $(FLAGS)

clean:
	rm -f *.o *.hi Roo