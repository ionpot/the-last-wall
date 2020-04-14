.PHONY: all ctags
all:
	ocamlbuild game/main.cma
ctags:
	ctags -R --exclude={.git,_build}
