prefix = ~/opt
cmd = stc-to-st stc-to-js

% : %.hs
	ghc -Wall -fwarn-tabs -O2 --make $<

all: $(cmd)

install: $(cmd)
	cp $(cmd) $(prefix)/bin

clean:
	rm -f *.hi *.o $(cmd)
