.PHONY: run
run: Main input.txt
	./Main < input.txt

Main: Main.hs
	ghc -Wall -Werror -o $@ -dynamic $<
