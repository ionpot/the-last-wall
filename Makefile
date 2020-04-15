.PHONY: all ctags
all:
	ocamlbuild -I util -r game/main.cma
ctags:
	ctags -R --exclude={.git,_build}
