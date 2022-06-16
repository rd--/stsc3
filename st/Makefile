# ui is for squeak
ui=Ui/String.ext Ui/Sc3Browser Ui/Sc3Editor

all:
	runhaskell ../hs/help.hs # gen Sc3HelpGraph.ext.som
	stsc3 translate directory som fileout ../som Stc3.st
	stsc3 translate directory som fileout Squeak Squeak.st
	stsc3 translate directory som fileout Gnu Gnu.st
	stsc3 translate directory som fileout Cuis Cuis.st
	stsc3 translate directory som fileout Ui Ui.st
	cat Stc3.st Squeak.st Ui.st > Sc3-Filein-For-Squeak.st
	cat Stc3.st Gnu.st > Sc3-Filein-For-Gnu.st
	cat Stc3.st Cuis.st > Sc3-Filein-For-Cuis.st

clean:
	rm -f Stc3.st
	rm -f Squeak.st Gnu.st Cuis.st Ui.st
	rm -f Sc3-Filein-For-*.st
