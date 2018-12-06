FLAGS?=SET_FLAGS_EXTERNALLY
OC=ocamlopt.opt
BUILDDIR=_build



$(BUILDDIR)/%.cmx : %.ml
	mkdir -p $(BUILDDIR)
	$(OC) $(FLAGS) -I $(BUILDDIR) -c -o $@ $<

naive : $(BUILDDIR)/shared.cmx $(BUILDDIR)/core.cmx $(BUILDDIR)/arith.cmx  $(BUILDDIR)/let.cmx $(BUILDDIR)/naive.cmx
	mkdir -p $(BUILDDIR)
	$(OC) $(FLAGS) -o $(BUILDDIR)/naive.exe $(BUILDDIR)/shared.cmx $(BUILDDIR)/core.cmx $(BUILDDIR)/arith.cmx  $(BUILDDIR)/let.cmx $(BUILDDIR)/naive.cmx



clean:
	rm -r $(BUILDDIR)