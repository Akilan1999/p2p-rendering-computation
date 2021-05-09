SHELL := /bin/bash

install:
	go build .
	./p2p-rendering-computation --SetDefaultConfig

build:
	go build .

run:
	go run main.go

