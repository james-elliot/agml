obin = $(shell opam var bin)
OCAMLC   = $(obin)/ocamlc
OCAMLOPT = $(obin)/ocamlopt
OCAMLDEP = $(obin)/ocamldep
OCAMLDOC = $(obin)/ocamldoc

parmapdir = $(shell opam var domainslib:lib)
INCLUDES = -I $(parmapdir)
#FLAGS_BIN = $(INCLUDES) -g -annot
FLAGS_BIN = $(INCLUDES) -g
FLAGS_OPT = $(INCLUDES) -unsafe -noassert -inline 100

FILES = ga_types.ml ga_cfg.ml ga_scale.ml ga_share.ml ga_crossmut.ml \
	ga_reproduce.ml ga_optimize.ml

OBJS_BIN = $(FILES:.ml=.cmo)
OBJS_OPT = $(FILES:.ml=.cmx)

DOC_DIR = doc/

all : libs

libs : ag.cma ag.cmxa


ag.cma : $(OBJS_BIN)
	$(OCAMLC) $(INCLUDES) -o $@ -a $(OBJS_BIN)

ag.cmxa : $(OBJS_OPT)
	$(OCAMLOPT) $(INCLUDES) -o $@ -a $(OBJS_OPT)

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo :
	$(OCAMLC) $(FLAGS_BIN) -c $<
.mli.cmi :
	$(OCAMLC) $(FLAGS_BIN) -c $<
.ml.cmx :
	$(OCAMLOPT) $(FLAGS_OPT) -c $<

doc : *.ml *.mli
	mkdir -p $(DOC_DIR)
	$(OCAMLDOC) $(INCLUDES) -html -d $(DOC_DIR) *.ml *.mli

cleanall : clean cleandoc

clean :
	rm -f *.cm* *.o *.a *.annot .depend *~

cleandoc :
	rm -rf $(DOC_DIR)

.depend :
	$(OCAMLDEP) *.mli *.ml >.depend

include .depend
