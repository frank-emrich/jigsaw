FLAGS?=SET_FLAGS_EXTERNALLY
OC=ocamlopt.opt
BUILDDIR=_build



$(BUILDDIR)/%.cmx : %.ml
	mkdir -p $(BUILDDIR)
	metaocamlopt $(FLAGS) -I $(BUILDDIR) -I +compiler-libs -c -o $@ $<


cached-runnative : $(BUILDDIR)/shared.cmx
	mkdir -p $(BUILDDIR)
	cp cached_runnative.ml $(BUILDDIR)
	metaocamlopt $(FLAGS) -c -I $(BUILDDIR) -I +compiler-libs  -o $(BUILDDIR)/cached_runnative.cmx $(BUILDDIR)/cached_runnative.ml

modular-staged : cached-runnative $(BUILDDIR)/shared.cmx $(BUILDDIR)/core_types.cmx $(BUILDDIR)/core.cmx $(BUILDDIR)/arith_types.cmx $(BUILDDIR)/arith.cmx  $(BUILDDIR)/let_types.cmx $(BUILDDIR)/let.cmx $(BUILDDIR)/modular_staged_types.cmx $(BUILDDIR)/modular_staged.cmx
	mkdir -p $(BUILDDIR)
	metaocamlopt $(FLAGS) -I $(BUILDDIR) -I ~/.opam/4.07.1+BER/lib/ocaml/compiler-libs -o $(BUILDDIR)/modular-staged.exe \
		$(BUILDDIR)/shared.cmx $(BUILDDIR)/cached_runnative.cmx  $(BUILDDIR)/core_types.cmx $(BUILDDIR)/core.cmx $(BUILDDIR)/arith_types.cmx $(BUILDDIR)/arith.cmx  $(BUILDDIR)/let_types.cmx $(BUILDDIR)/let.cmx $(BUILDDIR)/modular_staged_types.cmx $(BUILDDIR)/modular_staged.cmx


clean:
	rm -r $(BUILDDIR)




