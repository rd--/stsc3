all:
	runhaskell ../hs/help.hs # gen Sc3HelpGraph.ext.som
	stsc3 translate directory som fileout ../som/Sc3 Sc3.st
	stsc3 translate directory som fileout ../som/Osc Osc.st
	stsc3 translate directory som fileout ../som/Squeak Squeak.st
	stsc3 translate directory som fileout ../som/Gnu Gnu.st
	stsc3 translate directory som fileout ../som/Cuis Cuis.st
	stsc3 translate directory som fileout ../som/Ui Ui.st
	cat Osc.st Sc3.st Squeak.st Ui.st > Sc3-Filein-For-Squeak.st
	cat Osc.st Sc3.st Gnu.st Gnu/*.st > Sc3-Filein-For-Gnu.st
	cat Sc3.st Cuis.st > Sc3-Filein-For-Cuis.st

clean:
	rm -f Sc3.st Osc.st
	rm -f Squeak.st Gnu.st Cuis.st Ui.st
	rm -f Sc3-Filein-For-*.st
