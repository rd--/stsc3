GL_GIT=git@gitlab.com:rd--/stsc3.git
GL_HTTP=https://gitlab.com/rd--/stsc3.git

all:
	echo "stsc3"

clean:
	(cd cmd ; make clean)

push-gl:
	git push $(GL_GIT)

push-gl-tags:
	git push $(GH_GIT) --tag

pull-gl:
	git pull $(GL_HTTP)

update-rd:
	ssh rd@rohandrape.net "(cd sw/stsc3 ; git pull $(GL_HTTP))"

push-all:
	make push-gl update-rd
