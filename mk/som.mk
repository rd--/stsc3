%.ext.som : %.ext.st
	stsc3 translate extensions --tidy --sort fileout som $< $@

%.som : %.st
	stsc3 translate class --sort fileout som $< $@
