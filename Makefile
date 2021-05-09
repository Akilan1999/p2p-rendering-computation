SHELL := /bin/bash

install:
	go build .
	./p2p-rendering-computation --SetDefaultConfig

build:
	go build .

config:
	./p2p-rendering-computation --SetDefaultConfig

run:
	go run main.go

