SHELL := /bin/bash

install:
	go build -o p2prc
	echo '# Paths for p2p rendering and computation'
	echo 'export P2PRC=${PWD}'
	echo 'export PATH=${PWD}:$${PATH}'

build:
	go build -o p2prc

configfile:
	./p2prc --SetDefaultConfig

testcases:
	sh plugin/generate_test_case.sh

run:
	go run main.go

