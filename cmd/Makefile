prefix = ~/opt
cmd = stc-to-st stc-to-js # stsc3 is built by .cabal

% : %.hs
	ghc -Wall -fwarn-tabs -O2 --make $<

all: $(cmd)

install: $(cmd)
	cp $(cmd) $(prefix)/bin

clean:
	rm -fR dist
	rm -f *.hi *.o $(cmd)
	rm -f */*/*.hi */*/*.o
