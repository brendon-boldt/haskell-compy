default:
	@ghc -O -dynamic -isrc src/Main.hs
	@src/./Main $(source)

clean:
	rm Main src/*.{o,hs}
