# Installation 

Over here we will cover the basic steps to get the server and client side running. 

### Install Go lang 
The entire the implementation of this project is done using Go lang. 
Thus, we need go lang to compile to code to a binary file.
[Instructions to install Go lang](https://golang.org/doc/install)

### Install Docker 
In this project the choice of virtualization is Docker due to it's wide usage 
in the developer community. In the server module we use the Docker Go API to create and
interact with the containers. 

[Instructions to install docker](https://docs.docker.com/get-docker/)

[Instructions to install docker GPU](https://docs.nvidia.com/datacenter/cloud-native/container-toolkit/install-guide.html#docker)
````
Do ensure that the docker command does not need sudo to run
````

### Build Project and install project
To set up the internal dependencies and build the entire go code 
into a single binary
```
make install
```

### Test if binary works
```
p2prc --help
```
#### Output:
```
NAME:
   p2p-rendering-computation - p2p cli application to create and access VMs in other servers

USAGE:
   p2p-rendering-computation [global options] command [command options] [arguments...]

VERSION:
   1.0.0

COMMANDS:
   help, h  Shows a list of commands or help for one command

GLOBAL OPTIONS:
   --Mode value        Specifies mode of running (default: "client") [$P2P_MODE]
   --UpdateServerList  Update List of Server available based on servers iptables (default: false) [$UPDATE_SERVER_LIST]
   --ListServers       List servers which can render tasks (default: false) [$LIST_SERVERS]
   --CreateVM value    Creates Docker container on the selected server [$CREATE_VM]
   --RemoveVM value    Stop and Remove Docker container [$REMOVE_VM]
   --ID value          Docker Container ID [$ID]
   --Ports value       Number of ports to open for the Docker Container [$NUM_PORTS]
   --GPU               Create Docker Containers to access GPU (default: false) [$USE_GPU]
   --Specs value       Specs of the server node [$SPECS]
   --SetDefaultConfig  Sets a default configuration file (default: false) [$SET_DEFAULT_CONFIG]
   --help, -h          show help (default: false)
   --version, -v       print the version (default: false)


```



#### Note: The steps on how to use the commands will be added later.
