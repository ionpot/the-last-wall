FILES := steps.ml state.ml event.mli event.ml input_event.ml output_event.ml input.ml output.ml step.mli step.ml main.mli main.ml
.PHONY: all ctags
all:
	(cd game && ocamlc -c $(FILES))
ctags:
	ctags -R --exclude={.git,_build}
