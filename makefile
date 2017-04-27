
.PHONY: catoid clean

catoid :
	cd source && ocamlbuild main.native
	mv source/main.native catoid

clean :
	rm -f catoid
	cd source && ocamlbuild -clean

