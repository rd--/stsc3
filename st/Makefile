common=Base Core Env Event Haskell Help Help-Graphs Math Random UGen UGen-Demand UGen-Pseudo

gnu=$(common) Gnu UI
squeak=$(common) Squeak UI
cuis=$(common) Cuis

gnu_prefixed=$(addprefix SC3-, $(gnu))
squeak_prefixed=$(addprefix SC3-, $(squeak))
cuis_prefixed=$(addprefix SC3-, $(cuis))

all:
	cat $(gnu_prefixed:=.st) > Sc3-Filein-For-Gnu.st
	cat $(squeak_prefixed:=.st) > Sc3-Filein-For-Squeak.st
	cat $(squeak_prefixed:=.st) > Sc3-Filein-For-Cuis.st

clean:
	rm -f Sc3-Filein-For-*.st
