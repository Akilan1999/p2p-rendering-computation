SHELL := /bin/bash

install:
	$(shell mkdir /etc/p2p-rendering && chmod +r /etc/p2p-rendering && touch /etc/p2p-rendering/ip_table.json)

build:
	go build -o main main.go

run:
	go run main.go

