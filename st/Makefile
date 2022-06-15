include ../mk/som.mk

# ext are .ext.st files with extensions to standard classes
# classes are stsc3 defined classes
# autogen are the sc ugens and pseudo ugens and the help graphs
# ui is for squeak

ext = Array BlockClosure Collection Float Integer Interval Number Object SequenceableCollection String
classes = \
	ScUgen Ugen UgenBuilder PseudoUgen ScGraph Rate BinaryOpUgen UnaryOpUgen Control \
	Sc3 Sc3HelpGraph \
	EnvBuilder EnvAdsr EnvAsr EnvLinen EnvPerc EnvSine EnvSpec \
	EventParam Voicer
common=$(ext:=.ext) $(classes) $(pseudo)
autogen=Sc3-Ugen-Bindings.lib Sc3-Ugen-Pseudo.lib Sc3HelpGraph.ext
osc = Osc/Array.ext Osc/Float.ext Osc/Integer.ext Osc/String.ext Osc/OscMessage
ui=Ui/String.ext Ui/Sc3Browser Ui/Sc3Editor

all:
	runhaskell ../hs/pseudo.hs
	cat $(common:=.st) $(autogen:=.st) $(osc:=.st) Gnu/*.ext.st > Sc3-Filein-For-Gnu.st
	cat $(common:=.st) $(autogen:=.st) $(osc:=.st) $(ui:=.st) Squeak/*.ext.st Squeak/*.mod.st > Sc3-Filein-For-Squeak.st
	cat $(common:=.st) $(autogen:=.st) $(osc:=.st) Cuis/*.ext.st > Sc3-Filein-For-Cuis.st

clean:
	rm -f Sc3-Ugen-Pseudo.lib.st Sc3-Help-Graphs.st
	rm -f Sc3-Filein-For-*.st

translate-bindings:
	stsc3 translate library --tidy --sort fileout som Sc3-Ugen-Bindings.lib.st .

translate-pseudo:
	stsc3 translate library --tidy --sort fileout som Sc3-Ugen-Pseudo.lib.st .
