.PHONY: run
run: Main
	./$< < input.txt

Main: Main.hs
	ghc -Wall -Werror -dynamic -o $@ $<
