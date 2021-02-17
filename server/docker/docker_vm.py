#!/usr/bin/env python3

'''
FUTURE RELEASE TO BE CONVERTED TO GO LANG
'''

import click
import sys
import docker
import socket
import uuid 
import os

# ----------------------------------------------- CLI flags --------------------------------------------------------

@click.command()
@click.option("--createvm", is_flag=True, help="Creates docker default VM")

# -------------------------------------------------------------------------------------------------------------------
# -------------------------------------- Actions when flags are called ----------------------------------------------

def main(createvm):

    # creates docker virtual machine 
    if createvm:
        name = str(uuid.uuid4())
        build_run_contianer(name)

    else:
        ctx = click.get_current_context()
        click.echo(ctx.get_help())

# -------------------------------------------------------------------------------------------------------------------
# ------------------------------------------ build and run Contianer ------------------------------------------------

def build_run_contianer(name):
    # Get docker information from environment variables
    client = docker.from_env()

    image_path = "./server/docker/containers/docker-ubuntu-sshd/"
    tag_name = "p2p-ubuntu"
    free_port = str(get_free_tcp_port())

    # Change to JSON response 
    vnc_free_port = str(get_free_tcp_port())

    # Check if image is already exists (If already exists then delete and rebuild)
    try:
       client.images.get(tag_name)
       print("------ Image exists (Running Docker container " + name + ") --------")

       # Run the docker continer
       # Running interactive version until log files for results are created
       os.system("docker run -d=true --name="+ name +" --restart=always -p 6901:6901 -p "+ free_port +":22 -v=/opt/data:/data "+ tag_name +" /start")

    except:
       print("------ Image does not exists (building " + name + " and running image) ---------")

       # client.images.build(path=framework_image_path,tag=tag_name,rm=True)
       os.system("docker build -t " + tag_name + " " + image_path)

       print("------ Running Docker contianer " + name + " ---------")
       # Run the docker continer
       # Running interactive version until log files for results are created
       os.system("docker run -d=true --name="+ name +" --restart=always -p 6901:6901 -p "+ free_port +":22 -v=/opt/data:/data "+ tag_name +" /start")

# -------------------------------------------------------------------------------------------------------------------
# ------------------------------------------ Get free TCP port ------------------------------------------------------

def get_free_tcp_port():
     tcp = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
     tcp.bind(('', 0))
     addr, port = tcp.getsockname()
     tcp.close()
     return port


# -------------------------------------------------------------------------------------------------------------------

if __name__ == "__main__":
    main()