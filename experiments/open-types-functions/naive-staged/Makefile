FLAGS?=SET_FLAGS_EXTERNALLY
OC=ocamlopt.opt
BUILDDIR=_build



$(BUILDDIR)/%.cmx : %.ml
	mkdir -p $(BUILDDIR)
	metaocamlopt $(FLAGS) -I $(BUILDDIR) -I /home/frank/.opam/4.07.1+BER/lib/ocaml/compiler-libs -c -o $@ $<


custom-runnative :
	mkdir -p $(BUILDDIR)
	cp custom_runnative.ml $(BUILDDIR)
	metaocamlopt $(FLAGS) -c -o "-I $(BUILDDIR)" -I /home/frank/.opam/4.07.1+BER/lib/ocaml/compiler-libs -o $(BUILDDIR)/custom_runnative.cmx $(BUILDDIR)/custom_runnative.ml

naive-staged : custom-runnative $(BUILDDIR)/shared.cmx $(BUILDDIR)/core_types.cmx $(BUILDDIR)/core.cmx $(BUILDDIR)/arith_types.cmx $(BUILDDIR)/arith.cmx  $(BUILDDIR)/let_types.cmx $(BUILDDIR)/let.cmx $(BUILDDIR)/naive_staged_types.cmx $(BUILDDIR)/naive_staged.cmx
	mkdir -p $(BUILDDIR)
	metaocamlopt $(FLAGS) -I $(BUILDDIR) -I /home/frank/.opam/4.07.1+BER/lib/ocaml/compiler-libs -o $(BUILDDIR)/naive-staged.exe \
		$(BUILDDIR)/custom_runnative.cmx $(BUILDDIR)/shared.cmx $(BUILDDIR)/core_types.cmx $(BUILDDIR)/core.cmx $(BUILDDIR)/arith_types.cmx $(BUILDDIR)/arith.cmx  $(BUILDDIR)/let_types.cmx $(BUILDDIR)/let.cmx $(BUILDDIR)/naive_staged_types.cmx $(BUILDDIR)/naive_staged.cmx


naive-staged-combined :
	mkdir -p $(BUILDDIR)
	cp *.ml $(BUILDDIR)
	cd $(BUILDDIR) ; \
	opam switch 4.07.1+BER ; \
	eval `opam env` ; \
	metaocamlopt $(FLAGS) -I $(BUILDDIR) -o naive-staged-combined.exe shared.ml core_types.ml core.ml let_types.ml let.ml arith_types.ml arith.ml naive_staged_types.ml naive_staged.ml

clean:
	rm -r $(BUILDDIR)



