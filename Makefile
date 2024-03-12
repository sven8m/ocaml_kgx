

update_dune:
	dune uninstall kplacement
	dune install kplacement
	dune uninstall kdata
	dune install kdata
	dune uninstall kgraph
	dune install kgraph

.PHONY: update_dune
