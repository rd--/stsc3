all:
	(for i in *.sl ; do cat $$i | awk 'NR==1{gsub("#", "-"); print}' ; done) | LC_COLLATE=C sort -d
