SHELL := /bin/bash

.PHONY: set_virtualenv,install_docker_requirements,dockerproc

set_virtualenv:
	virtualenv env

install_docker_requirements:
	source env/bin/activate && pip install -r requirements.txt
    
dockerproc:
	ADMIN_USER=admin ADMIN_PASSWORD=admin docker-compose -f dockprom/docker-compose.yml up -d