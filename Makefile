all:
	echo "stsc3"

clean:
	rm -Rf dist dist-newstyle *~
	(cd cmd ; make clean)
	(cd Language/Smalltalk/Ansi ; make clean)
	(cd Language/Smalltalk/SuperCollider ; make clean)

push-all:
	r.gitlab-push.sh stsc3
	r.github-push.sh stsc3
