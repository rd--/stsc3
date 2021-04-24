GL_GIT=git@gitlab.com:rd--/stsc3.git
GL_HTTP=https://gitlab.com/rd--/stsc3.git

GH_GIT=git@github.com:rd--/stsc3.git
GH_HTTP=https://github.com/rd--/stsc3.git

all:
	echo "stsc3"

clean:
	echo "stsc3"

push-gl:
	git push $(GL_GIT)

push-gl-tags:
	git push $(GH_GIT) --tag

pull-gl:
	git pull $(GL_HTTP)

push-gh:
	git push $(GH_GIT)

push-gh-tags:
	git push $(GH_GIT) --tag

pull-gh:
	git pull $(GH_HTTP)

update-rd:
	ssh rd@rohandrape.net "(cd sw/stsc3 ; git pull $(GL_HTTP))"

push-all:
	make push-gl push-gh update-rd
