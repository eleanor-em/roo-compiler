PROXY = proxy.unimelb.edu.au:8000

all: Roo

Roo: Common.hs Oz.hs Roo.hs RooAnalyse.hs RooAst.hs RooCompile.hs RooParser.hs RooPrettyPrinter.hs SymTable.hs
	http_proxy=$(PROXY) https_proxy=$(PROXY) cabal build && \
		cp "$$(find . -type f -path './dist*' -name 'Roo')" .

clean:
	rm -f *.o *.hi Roo
