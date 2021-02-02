build:
	go build -o bin/main main.go
	ADMIN_USER=admin ADMIN_PASSWORD=admin docker-compose -f server/docker/dockprom/docker-compose.yml up -d

run:
	go run main.go
    ADMIN_USER=admin ADMIN_PASSWORD=admin docker-compose -f server/docker/dockprom/docker-compose.yml up -d