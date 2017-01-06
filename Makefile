

all: build-haskell

.PHONY: build-haskell
build-haskell:
	cd haskell && stack install 
	touch log
	@echo Type '`ether log`' to start up webapp

run-nginx: 
	nginx -c $(PWD)/nginx.conf -s reload
	ether log
