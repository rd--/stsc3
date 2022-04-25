core=Base Core Env Event Help Help-Graphs Math Random Ugen Ugen-Demand Ugen-Pseudo
base=Haskell
common=$(core) $(base)

gnu=$(common) Gnu Ui
squeak=$(common) Squeak Ui
cuis=$(common) Cuis

gnu_prefixed=$(addprefix Sc3-, $(gnu))
squeak_prefixed=$(addprefix Sc3-, $(squeak))
cuis_prefixed=$(addprefix Sc3-, $(cuis))

all:
	cat $(gnu_prefixed:=.st) > Sc3-Filein-For-Gnu.st
	cat $(squeak_prefixed:=.st) > Sc3-Filein-For-Squeak.st
	cat $(squeak_prefixed:=.st) > Sc3-Filein-For-Cuis.st

clean:
	rm -f Sc3-Filein-For-*.st
