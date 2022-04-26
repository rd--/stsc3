std=Array BlockClosure Boolean Collection Dictionary Float Integer Interval Number SequenceableCollection String Symbol
core=Env Event Help Help-Graphs
ugen=AbstractUgen Ugen BinaryOpUgen UnaryOpUgen Ugen-Bindings Ugen-Demand Ugen-Pseudo Mrg Mce Mix Constant Control Rate OverlapTexture
base=Sc3 Haskell
common=$(std) $(core) $(ugen) $(base)

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
