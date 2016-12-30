

all: build-haskell

.PHONY: build-haskell
build-haskell:
	stack install 
	touch log
	@echo Type '`tail -f log | ether log`' to start up webapp

