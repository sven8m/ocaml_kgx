all:
	dune build
	mv okgx.exe okgx

clean:
	dune clean
	rm -f okgx


.PHONY: all clean
