obin = $(shell opam var bin)
OCAMLC   = $(obin)/ocamlc
OCAMLOPT = $(obin)/ocamlopt
OCAMLDEP = $(obin)/ocamldep
OCAMLDOC = $(obin)/ocamldoc

parmapdir = $(shell opam var parmap:lib)
INCLUDES = -I $(parmapdir) -I ..
#FLAGS_BIN = $(INCLUDES) -g -annot
FLAGS_BIN = $(INCLUDES) -g
FLAGS_OPT = $(INCLUDES) -unsafe -noassert -inline 100

FILES=  main.ml
INTERF = $(FILES:.mli=.cmi)
NORM_OBJS =  $(FILES:.ml=.cmo)
OPT_OBJS=  $(FILES:.ml=.cmx)

LIBS=  str.cma unix.cma parmap.cma ag.cma 

all : main.opt main.norm

main.opt :  $(OPT_OBJS) 
	$(OCAMLOPT) $(FLAGS_OPT) -o $@ $(LIBS:.cma=.cmxa) $(OPT_OBJS) 

main.norm :  $(NORM_OBJS) 
	$(OCAMLC) $(FLAGS_BIN) -o $@ $(LIBS) $(NORM_OBJS) 

.SUFFIXES: .mll .mly .ml .mli .cmo .cmi .cmx

.ml.cmo :
	$(OCAMLC) $(FLAGS_BIN) -c $<
.mli.cmi :
	$(OCAMLC) $(FLAGS_BIN) -c $<
.ml.cmx :
	$(OCAMLOPT) $(FLAGS_OPT) -c $<

clean:
	\rm -f *.cmo *.cmi *.cmx *.o *.a *.cma *.cmxa *.opt *.norm *~

