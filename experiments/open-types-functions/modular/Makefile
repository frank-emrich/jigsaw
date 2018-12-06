FLAGS?=SET_FLAGS_EXTERNALLY
OC=ocamlopt.opt
BUILDDIR=_build



$(BUILDDIR)/%.cmx : %.ml
	mkdir -p $(BUILDDIR)
	$(OC) $(FLAGS) -I $(BUILDDIR) -c -o $@ $<

modular : $(BUILDDIR)/shared.cmx $(BUILDDIR)/core.cmx $(BUILDDIR)/arith.cmx  $(BUILDDIR)/let.cmx $(BUILDDIR)/modular.cmx
	mkdir -p $(BUILDDIR)
	$(OC) $(FLAGS) -o $(BUILDDIR)/modular.exe $(BUILDDIR)/shared.cmx $(BUILDDIR)/core.cmx $(BUILDDIR)/arith.cmx  $(BUILDDIR)/let.cmx $(BUILDDIR)/modular.cmx




clean:
	rm -r $(BUILDDIR)