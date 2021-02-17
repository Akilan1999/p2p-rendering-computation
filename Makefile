SHELL := /bin/bash

build:
	go build -o main main.go

run:
	go run main.go

set_virtualenv:
	virtualenv p2p-rendering

install_docker_requirements:
	source p2p-rendering/bin/activate && pip install -r server/docker/requirements.txt
    

dockerproc:
	ADMIN_USER=admin ADMIN_PASSWORD=admin docker-compose -f server/docker/dockprom/docker-compose.yml up -d