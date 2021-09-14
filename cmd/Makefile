prefix = ~/opt
cmd = stsc3

% : %.hs
	ghc -Wall -fwarn-tabs -O2 --make $<

all: $(cmd)

install: $(cmd)
	cp $(cmd) $(prefix)/bin

clean:
	rm -f *.hi *.o $(cmd)
	rm -f */*/*.hi */*/*.o
