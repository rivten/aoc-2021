.PHONY: run

run: Main
	./Main < input.txt

Main: Main.hs
	ghc -Wall -Werror -dynamic $<
