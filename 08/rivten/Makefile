.PHONY: run

run: Main
	./$< < input.txt

Main: Main.hs
	ghc -dynamic -Wall -Werror $<
