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
autogen=Sc3-Ugen-Bindings Sc3-Ugen-Pseudo Sc3-Help-Graphs
osc = Osc/Array.ext Osc/Float.ext Osc/Integer.ext Osc/String.ext Osc/OscMessage
ui=Ui/String.ext Ui/Sc3Browser Ui/Sc3Editor

all:
	runhaskell ../hs/help.hs
	runhaskell ../hs/pseudo.hs
	cat $(common:=.st) $(autogen:=.st) $(osc:=.st) Gnu/*.ext.st > Sc3-Filein-For-Gnu.st
	cat $(common:=.st) $(autogen:=.st) $(osc:=.st) $(ui:=.st) Squeak/*.ext.st > Sc3-Filein-For-Squeak.st
	cat $(common:=.st) $(autogen:=.st) $(osc:=.st) Cuis/*.ext.st > Sc3-Filein-For-Cuis.st

clean:
	rm -f Sc3-Ugen-Pseudo.st Sc3-Help-Graphs.st
	rm -f Sc3-Filein-For-*.st
