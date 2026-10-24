cmd = spl-to-st spl-to-js

% : %.hs
	ghc -Wall -fwarn-tabs -O2 --make $<
	rm $@.hi $@.o

all: $(cmd)

clean:
	rm -f *.hi *.o $(cmd)
