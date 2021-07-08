.PHONY: run

run:
	stack build
	stack exec hello-world-exe
