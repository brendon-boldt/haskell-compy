
SOURCES = $(wildcard src/*.hs)

.PHONY: default
default:
	@rm -f asm/main.s
	@stack build
	@stack exec haskell-compy-exe $(source)
	@gcc -o asm/main.out asm/main.s asm/lib.s
	@asm/./main.out

.PHONY: profile
profile:
	@rm -f asm/main.s
	@stack build --profile
	@stack exec -- haskell-compy-exe $(source) +RTS -h -p
