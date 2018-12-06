FLAGS?=SET_FLAGS_EXTERNALLY
OC=ocamlopt.opt
BUILDDIR=_build




normal_datatypes:
	mkdir -p $(BUILDDIR)
	cp *.ml $(BUILDDIR)
	cd $(BUILDDIR) ; \
	$(OC) $(FLAGS) -o normal_datatypes.exe normal_datatypes.ml


polymorphic_variants:
	mkdir -p $(BUILDDIR)
	cp *.ml $(BUILDDIR)
	cd $(BUILDDIR) ; \
	$(OC) $(FLAGS) -o polymorphic_variants.exe polymorphic_variants.ml


clean:
	rm -r $(BUILDDIR)