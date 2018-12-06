FLAGS?=SET_FLAGS_EXTERNALLY
OC=ocamlopt.opt
BUILDDIR=_build



$(BUILDDIR)/%.cmx : %.ml
	mkdir -p $(BUILDDIR)
	$(OC) $(FLAGS) -I $(BUILDDIR) -c -o $@ $<

modular : $(BUILDDIR)/shared.cmx $(BUILDDIR)/core.cmx $(BUILDDIR)/arith.cmx  $(BUILDDIR)/let.cmx $(BUILDDIR)/modular.cmx
	mkdir -p $(BUILDDIR)
	$(OC) $(FLAGS) -o $(BUILDDIR)/modular.exe $(BUILDDIR)/shared.cmx $(BUILDDIR)/core.cmx $(BUILDDIR)/arith.cmx  $(BUILDDIR)/let.cmx $(BUILDDIR)/modular.cmx



modular_combined_compilation:
	mkdir -p $(BUILDDIR)
	cp *.ml $(BUILDDIR)
	cd $(BUILDDIR) ; \
	$(OC) $(FLAGS) -o modular_combined_compilation.exe shared.ml core.ml arith.ml let.ml modular.ml
	cd ..


clean:
	rm -r $(BUILDDIR)