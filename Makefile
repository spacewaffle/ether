

all: build-haskell

.PHONY: build-haskell
build-haskell:
	stack install 
	@echo Type '`ether`' to start up webapp

