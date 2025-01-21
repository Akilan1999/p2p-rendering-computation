# Installation

| [◀ Previous](Introduction.md) | [Next ▶](Abstractions.md) |
|:-----------:|---------|

Over here we will cover the basic steps to get the server and client side running. 

## Latest release install 
https://github.com/Akilan1999/p2p-rendering-computation/releases

## Install from Github master branch 

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
// Do ensure that the docker command does not need sudo to run
sudo chmod 666 /var/run/docker.sock
````

### Build Project and install project
To set up the internal dependencies and build the entire go code 
into a single binary
```
make install
```

#### For Windows
To set up P2PRC on Windows, simply run this batch file.
**Make sure you are not in admin mode when running this.**
```
.\install.bat
```

### Add appropriate paths to `.bashrc`
```
export P2PRC=/<PATH>/p2p-rendering-computation
export PATH=/<PATH>/p2p-rendering-computation:${PATH}
```

### Set up configuration file
```
make configfile 
```
Open the config file ```config.json``` and add the IPv6 address 
if you have one. 

### Test if binary works
```
p2prc --help
```
#### Output:
```
NAME:
   p2p-rendering-computation - p2p cli application to create and access VMs in other servers

USAGE:
   p2prc [global options] command [command options] [arguments...]

VERSION:
   <version no>

COMMANDS:
   help, h  Shows a list of commands or help for one command

GLOBAL OPTIONS:
   --Server, -s                           Starts server (default: false) [$SERVER]
   --UpdateServerList, --us               Update List of Server available based on servers iptables (default: false) [$UPDATE_SERVER_LIST]
   --ListServers, --ls                    List servers which can render tasks (default: false) [$LIST_SERVERS]
   --AddServer value, --as value          Adds server IP address to iptables [$ADD_SERVER]
   --ViewImages value, --vi value         View images available on the server IP address [$VIEW_IMAGES]
   --CreateVM value, --touch value        Creates Docker container on the selected server [$CREATE_VM]
   --ContainerName value, --cn value      Specifying the container run on the server side [$CONTAINER_NAME]
   --RemoveVM value, --rm value           Stop and Remove Docker container (IP:port) accompanied by container ID via --ID or --id [$REMOVE_VM]
   --ID value, --id value                 Docker Container ID [$ID]
   --Ports value, -p value                Number of ports to open for the Docker Container [$NUM_PORTS]
   --GPU, --gpu                           Create Docker Containers to access GPU (default: false) [$USE_GPU]
   --Specification value, --specs value   Specs of the server node [$SPECS]
   --SetDefaultConfig, --dc               Sets a default configuration file (default: false) [$SET_DEFAULT_CONFIG]
   --NetworkInterfaces, --ni              Shows the network interface in your computer (default: false) [$NETWORK_INTERFACE]
   --ViewPlugins, --vp                    Shows plugins available to be executed (default: false) [$VIEW_PLUGIN]
   --TrackedContainers, --tc              View (currently running) containers which have been created from the client side  (default: false) [$TRACKED_CONTAINERS]
   --ExecutePlugin value, --plugin value  Plugin which needs to be executed [$EXECUTE_PLUGIN]
   --CreateGroup, --cgroup                Creates a new group (default: false) [$CREATE_GROUP]
   --Group value, --group value           group flag with argument group ID [$GROUP]
   --Groups, --groups                     View all groups (default: false) [$GROUPS]
   --RemoveContainerGroup, --rmcgroup     Remove specific container in the group (default: false) [$REMOVE_CONTAINER_GROUP]
   --RemoveGroup value, --rmgroup value   Removes the entire group [$REMOVE_GROUP]
   --Generate value, --gen value          Generates a new copy of P2PRC which can be modified based on your needs [$GENERATE]
   --ModuleName value, --mod value        New go project module name [$MODULENAME]
   --PullPlugin value, --pp value         Pulls plugin from git repos [$PULLPLUGIN]
   --RemovePlugin value, --rp value       Removes plugin [$REMOVEPLUGIN]
   --help, -h                             show help (default: false)
   --version, -v                          print the version (default: false)

```

<br>

--------------

<br>

# Using basic commands 

### Start as a server 
Do ensure you have Docker installed for this 
```
p2prc -s 
```

### View server Specification 
```
p2prc --specs=<ip address>
```

### Run container 
use the ```--gpu``` if you know the other machine has a gpu. 
```
p2prc --touch=<server ip address> -p <number of ports> --gpu
```

### Remove container 
The docker id is present in the output where you create a container
```
p2prc --rm=<server ip address> --id=<docker container id> 
```

### Adding servers to ip table
```
p2prc --as=<server ip address you want to add> 
```

### Update ip table 
```
p2prc --us 
```

### List Servers 
```
p2prc --ls 
```

### View Network interfaces 
```
p2prc --ni
```

### Viewing Containers created Client side 
```
p2prc --tc
```
[read more on tracking containers](ClientImplementation.md#tracking-containers)

### Running plugin 
```
p2prc --plugin <plugin name> --id <container id or group id>
``` 

### Create group
```
p2prc --cgroup
```
### Add container to group
```
p2prc --group <group id> --id <container id>
``` 
### View groups
```
p2prc --groups
```
### View specific group
```
p2prc --group <group id>
```
### Delete container from group
```
p2prc --rmcgroup --group <group id> --id <container id> 
```
### Delete entire group
```
p2prc --rmgroup <group id>
```
[read more on grouping containers](ClientImplementation.md#Grouping-Containers)
### Extending usecase of P2PRC (Requires a go compiler to run)
```
p2prc --gen <project name> --mod <go module name>
```
[read more about the generate module](GenerateImplementation.md)

### Pulling plugin from a remote repo 
```
p2prc --pp <repo link>
```

### Deleting plugin from the plugin directory
```
p2prc --rp <plugin name> 
```

### Added custom metadata about the current node
```
p2prc --amd "custom metadata"
```

<br>

--------------

<br>

# Using Plugins 
This feature is still Under Development:
[Read more on the implementation](PluginImplementation.md)

#### Dependencies
- Ansible:
  - Debian/ubuntu: ```sudo apt install ansible```
  - Others: [Installation link](https://ansible-tips-and-tricks.readthedocs.io/en/latest/ansible/install/)
  
#### Run Test Cases 
- Generate Test Case Ansible file 
  - ```make testcases```
- Enter inside plugin directory and run tests.<br>

> [!NOTE]
> That docker needs to installed and needs to run without
> sudo. Refer the section [Install Docker](#install-docker). 
> - ```cd plugin```
> - ```go test .```

---
  
### Next Chapter: [Abstractions](Abstractions.md)
