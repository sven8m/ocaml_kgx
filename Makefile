all:
	dune build
	mv otgx.exe otgx

clean:
	dune clean
	rm -f otgx


.PHONY: all clean
