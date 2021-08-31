SHELL := /bin/bash

install:
	sh install.sh p2prc

testcases:
	sh plugin/generate_test_case.sh

run:
	go run main.go

