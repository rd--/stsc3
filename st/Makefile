# ext are extensions to standard classes
# classes are stsc3 defined classes
# pseudo are pseudo ugens, these should be autogened into a single file, see Sc3-Ugen-Pseudo.stc
# autogen are the primitive ugens and the help graphs
# ui is for squeak
ext = Array BlockClosure Boolean Collection Dictionary False Float Fraction Integer Interval Number SequenceableCollection String Symbol True
classes = \
	AbstractUgen EnvBuilder PseudoUgen Ugen \
	BinaryOpUgen Constant Control Env EnvADSR EnvASR EnvLinen EnvPerc EnvSine EnvSpec EventParam KlankSpec Mce Mix Mrg OverlapTexture Rate Sc3 Sc3HelpGraph UnaryOpUgen Voicer
pseudo=ADSR ASR AudioIn BufAlloc BufRec Cc Changed Choose ControlIn Cutoff DmdFor DmdOn DynKlank InFb KeyDown KeyPitch KeyTimbre KeyVelocity LinLin LinSeg Ln Osc1 PMOsc PenAngle PenDown PenRadius PenX PenY PenZ RingzBank Select2 SelectX Seq Ser Shuf SinOscBank Splay Splay2 Sw TChoose TDmdFor TLine TXLine Tr Tr1 XLn
common=$(ext:=.ext) $(classes) $(pseudo)
autogen=Sc3-Ugen-Bindings Sc3-Help-Graphs
ui=Sc3-Ui

all:
	cat $(common:=.st) > Sc3-Filein-For-Common.st
	cat $(common:=.st) $(autogen:=.st) Gnu/*.st > Sc3-Filein-For-Gnu.st
	cat $(common:=.st) $(autogen:=.st) $(ui:=.st) Squeak/*.st > Sc3-Filein-For-Squeak.st
	cat $(common:=.st) $(autogen:=.st) Cuis/*.st > Sc3-Filein-For-Cuis.st

clean:
	rm -f Sc3-Filein-For-*.st
