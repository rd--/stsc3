all:
	runhaskell ../hs/help.hs # gen both som/Sc3-Help/Sc3HelpGraph.ext.som and st/Sc3-Help.st
	stsc3 translate directory --category=Sound-Sc3 som fileout ../som/Sc3-Kernel Sc3-Kernel.st
	stsc3 translate directory --category=Sound-Sc3 som fileout ../som/Sc3-Ugen Sc3-Ugen.st
	stsc3 translate directory --category=Sound-Sc3 som fileout ../som/Sc3-Pseudo Sc3-Pseudo.st
#	stsc3 translate directory --category=Sound-Sc3 som fileout ../som/Sc3-Help Sc3-Help.st
	stsc3 translate directory --category=Sound-Osc som fileout ../som/Osc-Kernel Osc-Kernel.st
	stsc3 translate directory --category=Sound-Sc3 som fileout ../som/Squeak Squeak.st
	stsc3 translate directory --category=Sound-Sc3 som fileout ../som/Gnu Gnu.st
	stsc3 translate directory --category=Sound-Sc3 som fileout ../som/Cuis Cuis.st
	stsc3 translate directory --category=Sound-Sc3 som fileout ../som/Ui Ui.st
	cat Sc3-Kernel.st Sc3-Ugen.st Sc3-Pseudo.st Sc3-Help.st > Sc3.st
	cat Osc-Kernel.st Sc3.st Squeak.st Ui.st > Sc3-Filein-For-Squeak.st
	cat Osc-Kernel.st Sc3.st Gnu.st Gnu/*.st > Sc3-Filein-For-Gnu.st
	cat Osc-Kernel.st Sc3.st Cuis.st > Sc3-Filein-For-Cuis.st

clean:
	rm -f Osc-Kernel.st Sc3-*.st Sc3.st
	rm -f Squeak.st Gnu.st Cuis.st Ui.st
	rm -f Sc3-Filein-For-*.st
