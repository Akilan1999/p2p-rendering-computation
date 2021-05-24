SHELL := /bin/bash

install:
	go build -o p2prc
	echo '# Paths for p2p rendering and computation' >> ~/.bashrc
	echo 'export P2PRC=${PWD}' >> ~/.bashrc
	echo 'export ${PWD}:$${PATH}' >> ~/.bashrc
	source ~/.bashrc
	./p2prc --SetDefaultConfig


build:
	go build -o p2prc

config:
	./p2p-rendering-computation --SetDefaultConfig

run:
	go run main.go

