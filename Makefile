SHELL := /bin/bash

install:
	sh install.sh p2prc

testcases:
	sh plugin/generate_test_case.sh

run:
	go run main.go

sharedObjects:
	sh build-bindings.sh

python:
	sh build-python-package.sh

clean:
	go clean -modcache
	rm -fr .go-build vendor result*
