last_tag := $(shell git describe --abbrev=0)
opam_switch := $(shell opam switch show)
is32bit := $(findstring 32bit,$(opam_switch))
suffix := $(if $(is32bit),_32,)
exe_file := _build/default.windows/tty/main.exe
dst ?= .

.PHONY: all copy ctags win
all:
	dune build @all
copy:
	cp ${exe_file} ${dst}/${last_tag}${suffix}.exe
ctags:
	ctags -R --exclude={.git,_build}
win:
	dune build @all -x windows
