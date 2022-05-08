# ext are extensions to standard classes
# classes are stsc3 defined classes
# autogen are the sc ugens and pseudo ugens and the help graphs
# ui is for squeak
ext = Array BlockClosure Boolean Collection Dictionary Float Integer Interval Number Object SequenceableCollection Stream String
classes = \
	EnvBuilder ScUgen Ugen UgenBuilder PseudoUgen ScGraph \
	BinaryOpUgen Control Env EnvAdsr EnvAsr EnvLinen EnvPerc EnvSine EnvSpec EventParam KlankSpec OverlapTexture Rate Sc3 Sc3Id Sc3HelpGraph UnaryOpUgen Voicer
common=$(ext:=.ext) $(classes) $(pseudo)
autogen=Sc3-Ugen-Bindings Sc3-Ugen-Pseudo Sc3-Help-Graphs
ui=Sc3-Ui

all:
	runhaskell ../hs/help.hs
	runhaskell ../hs/pseudo.hs
	cat $(common:=.st) > Sc3-Filein-For-Common.st
	cat $(common:=.st) $(autogen:=.st) Gnu/*.st > Sc3-Filein-For-Gnu.st
	cat $(common:=.st) $(autogen:=.st) $(ui:=.st) Squeak/*.st > Sc3-Filein-For-Squeak.st
	cat $(common:=.st) $(autogen:=.st) Cuis/*.st > Sc3-Filein-For-Cuis.st

clean:
	rm -f Sc3-Ugen-Pseudo.st Sc3-Help-Graphs.st
	rm -f Sc3-Filein-For-*.st
