.PHONY: all ctags
all:
	dune build @all
ctags:
	ctags -R --exclude={.git,_build}
