
SOURCES = $(wildcard src/*.hs)

.PHONY: default
default: $(SOURCES)
	@rm -f asm/main.s
	@ghc -O -dynamic -isrc src/Main.hs
	@src/./Main $(source)
	@gcc -o Main asm/main.s asm/lib.s
	@./Main

.PHONY: asm
asm: src/Asm.hs
	@ghc -O -dynamic -isrc src/Asm.hs
	@rm asm/main.s
	@src/./Asm
	@gcc -o Main asm/main.s asm/lib.s
	@./Main

clean:
	rm Main src/*.{o,hi}
