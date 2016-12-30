

all: build-haskell
	

.PHONY: run
run: 
	ether

.PHONY: build-haskell
build-haskell:
	stack install 
	@echo Type '`make run`' to start up webapp

