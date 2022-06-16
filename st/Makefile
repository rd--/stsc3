# ui is for squeak
ui=Ui/String.ext Ui/Sc3Browser Ui/Sc3Editor

all:
	runhaskell ../hs/help.hs # gen Sc3HelpGraph.ext.som
	stsc3 translate directory som fileout ../som Stc3-Fileout.st
	cat Stc3-Fileout.st Gnu/*.ext.st > Sc3-Filein-For-Gnu.st
	cat Stc3-Fileout.st Squeak/*.ext.st Squeak/*.mod.st > Sc3-Filein-For-Squeak.st
	cat Stc3-Fileout.st Cuis/*.ext.st > Sc3-Filein-For-Cuis.st

clean:
	rm -f Stc3-Fileout.st
	rm -f Sc3-Filein-For-*.st
