FLAGS?=SET_FLAGS_EXTERNALLY
OC=ocamlopt.opt
BUILDDIR=_build


combined:
	mkdir -p $(BUILDDIR)
	cp *.ml $(BUILDDIR)
	cd $(BUILDDIR) ; \
	$(OC) $(FLAGS) -o combined.exe combined.ml


clean:
	rm -r $(BUILDDIR)