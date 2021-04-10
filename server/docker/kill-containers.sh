docker kill $(docker ps -q)\
docker rm $(docker ps -a -q)
docker rmi $(docker images -q) --force
docker system prune --all
