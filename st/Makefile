std=Array BlockClosure Boolean Collection Dictionary Float Integer Interval Number SequenceableCollection String Symbol
core=Env Event Help Help-Graphs
ugen=AbstractUgen Ugen BinaryOpUgen UnaryOpUgen Ugen-Demand Ugen-Pseudo Mrg Mce Mix Constant Control Rate OverlapTexture
base=Sc3 Haskell
common=$(std) $(core) $(ugen) $(base)
autogen=Ugen-Bindings
ui=Ui

common_prefixed=$(addprefix Sc3-, $(common))
autogen_prefixed=$(addprefix Sc3-, $(autogen))
ui_prefixed=$(addprefix Sc3-, $(ui))

all:
	cat $(common_prefixed:=.st) > Sc3-Filein-For-Common.st
	cat $(common_prefixed:=.st) $(autogen_prefixed:=.st) Gnu/*.st > Sc3-Filein-For-Gnu.st
	cat $(common_prefixed:=.st) $(autogen_prefixed:=.st) $(ui_prefixed:=.st) Squeak/*.st > Sc3-Filein-For-Squeak.st
	cat $(common_prefixed:=.st) $(autogen_prefixed:=.st) Cuis/*.st > Sc3-Filein-For-Cuis.st

clean:
	rm -f Sc3-Filein-For-*.st
