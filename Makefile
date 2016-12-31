

all: build-haskell

.PHONY: build-haskell
build-haskell:
	stack install 
	touch log
	@echo Type '`ether log`' to start up webapp

