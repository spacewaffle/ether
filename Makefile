

all: build-haskell

.PHONY: build-haskell
build-haskell:
	stack install 
	@echo Type '`touch log && tail -f log | ether log`' to start up webapp

