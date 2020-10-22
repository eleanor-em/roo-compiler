FLAGS = -Wall -g -fno-warn-name-shadowing -fno-warn-unused-do-bind -fbreak-on-exception 
PACKAGE = -package parsec -package mtl
PROXY = proxy.unimelb.edu.au:8000

all: Roo

Roo: Common.hs Oz.hs Roo.hs RooAnalyse.hs RooAst.hs RooCompile.hs RooParser.hs RooPrettyPrinter.hs SymTable.hs
	http_proxy=$(PROXY) https_proxy=$(PROXY) cabal install rainbow --lib
	cabal install pretty-simple --lib
	ghc Roo.hs -o Roo $(PACKAGE) $(FLAGS)

clean:
	rm -f *.o *.hi Roo
