CAMLC = ocamlc
CAMLOPT = ocamlopt -p -inline 20 -nodynlink -unsafe -noassert
CAMLDOC = ocamldoc -html -keep-code
MKTOP = ocamlmktop
#CAMLLEX = ocamllex
#CAMLYACC = ocamlyacc

COMMANDS = \
	   -package unix \
#	   -package str \
#	   -package bigarray \
#	   -package camomile \
#	   -linkpkg 

LIBS = graphics.cma unix.cma
LIBS_OPT = $(patsubst %.cma,%.cmxa,$(LIBS))

EXEC = toto
MAIN = main
TEST = test
TEST_EXEC = .test
TOPLEVEL = toplevel

RAW_OBJS = param.cmo common.cmo print.cmo combinatoric.cmo rational.cmo field.cmo prettyprinting.cmo refine.cmo flag.cmo graphic.cmo algebra.cmo graph.cmo graph_mod.cmo digraph.cmo trianglefree.cmo storage.cmo sdp.cmo latexify.cmo vectors.cmo inequality.cmo solve.cmo
OBJS = $(addprefix ${COMPILE}/,${RAW_OBJS}) 
MAIN_OBJ = ${COMPILE}/${MAIN}.cmo
MAIN_OBJ_OPT = ${COMPILE}/${MAIN}.cmx
TEST_OBJ = ${COMPILE}/${TEST}.cmo
TEST_OBJ_OPT = ${COMPILE}/${TEST}.cmx
OBJS_OPT = $(patsubst %.cmo,%.cmx,$(OBJS))

COMPILE = compile
SRC = src
DOC = doc
LATEX = latex
INCLUDE = -I ${COMPILE} -I ${SRC}

.PHONY: all opt clean mrproper recycle fixm test top doc launch optlaunch latex
.PRECIOUS: $(COMPILE)/%.ml $(COMPILE)/%.mli # to prevent deletion as intermediate

all: .depend ${EXEC}

launch: all
	@./${EXEC}

optlaunch: opt
	@./${EXEC}.opt

#opt: ${EXEC}.opt

recycle:
	@rm -rf *~
	@rm -rf ${SRC}/*~

clean:
	@rm -rf ${COMPILE}/*
	@rm -rf .depend
	@rm -rf $(DOC)/*
	@rm -rf $(LATEX)/*

latex:
	@cd $(LATEX)/; pdflatex *.tex; evince *.pdf &

.SUFFIXES: .ml .mli .cmo .cmi .cmx .mll .mly

test: ${TEST_EXEC}
	@./${TEST_EXEC}

doc : $(OBJS)
	@$(CAMLDOC) $(INCLUDE) -d $(DOC) compile/*.ml compile/*.mli

opt: ${EXEC}.opt

top: ${TOPLEVEL}
	@./${TOPLEVEL} -I ${COMPILE}/

.${TEST}: ${OBJS} ${TEST_OBJ}
	@${CAMLC} ${LIBS} ${OBJS} ${TEST_OBJ} -o ${TEST_EXEC}

${EXEC}: ${OBJS} ${MAIN_OBJ}
	@${CAMLC} ${LIBS} ${OBJS} ${MAIN_OBJ} -o ${EXEC}

${EXEC}.opt: ${OBJS_OPT} ${MAIN_OBJ_OPT}
	@${CAMLOPT} ${LIBS_OPT} ${OBJS_OPT} ${MAIN_OBJ_OPT} -o ${EXEC}.opt

${TOPLEVEL}: ${OBJS}
	@${MKTOP} ${LIBS} -o ${TOPLEVEL} ${OBJS}

${COMPILE}/%.ml: ${SRC}/%.ml
	@cp $< $@

${COMPILE}/%.mli: ${SRC}/%.mli
	@cp $< $@

${COMPILE}/%.cmo: ${COMPILE}/%.ml
	@echo "\033[1;34m$*.ml → $*.cmo\033[0;0m"
	@${CAMLC} -I ${COMPILE} -c $<

${COMPILE}/%.cmx: ${COMPILE}/%.ml
	@echo "\033[1;34m$*.ml → $*.cmx\033[0;0m"
	@${CAMLOPT} -I ${COMPILE} -c $<

${COMPILE}/%.cmi: ${COMPILE}/%.mli
	@echo "\033[1;30m$*.mli → $*.cmi\033[0;0m"
	@${CAMLC} -I ${COMPILE} -c $<
.depend:
	@ocamldep -I ${SRC} ${SRC}/*.mli ${SRC}/*.ml > .depend.tmp
	@sed "s/${SRC}\/\([a-zA-Z0-9_]*\).cm\(.\)/${COMPILE}\/\1.cm\2/g" .depend.tmp > .depend

include .depend
