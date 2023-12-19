all:
	echo "stsc3"

clean:
	rm -Rf dist dist-newstyle cabal.project.local *~
	(cd cmd ; make clean)
	(cd Language/Smalltalk/Ansi ; make clean)
	(cd Language/Smalltalk/SuperCollider ; make clean)
	(cd som ; make clean)
	(cd st ; make clean)
	find . -name '*.o' -exec rm {} +
	find . -name '*.hi' -exec rm {} +

push-all:
	r.gitlab-push.sh stsc3
	r.github-push.sh stsc3

remote-update:
	ssh rd@rohandrape.net "(cd rohandrape.net/pub/stsc3 ; git pull)"

indent:
	fourmolu -i Language
