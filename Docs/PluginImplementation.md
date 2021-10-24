# Plugin Module Implementation 
## Topics
1. [Introduciton](#introduction)
2. [Site.yml](#site-File-Template)
3. [Host](#hosts-file)
4. [Description](#description-file)
5. [Automatic port allocations](#automatic-port-allocations)
6. [Sample plugins implemented](#sample-plugins-implemented)

## Introduction
    
The plugin module is designed to ensure clients can execute instructions in a declarative manner across different 
containers created. This means the user (i.e client) needs to write the instruction only once, and these instructions 
can be executed across different nodes in a repetitive manner. 

In the scenario of this project Ansibles will be used as the way the users can create these instructions. 

- [Setup instruction](Installation.md#Using-Plugins)

The plugin module introduces a new path to the config file known as pluginpath. This path by defaults points to
```${P2PRC}/plugin/deploy```. Any file/folder inside ```plugin/deploy``` is part of the .gitginore. Plugins are 
detected by folder names inside the ```plugin/deploy```. 
```
plugin
|___ Deploy 
       |___<plugin name>
                |___ site.yml 
                |___ hosts
                |___ ports.json
                |___ description.txt 
              .
              . 
              .
              n: n number of plugins possible 
```

## Site File Template
The site file is also known as the Ansible playbook and is incharge of executing 
instructions in a declarative manner. The below example specifies how to make one. 
```
- hosts: all

  tasks:
    - name: <task name> 
      <ansible task> 
      debug:
        msg: <debug message> 
```
Read more about ansible tasks: https://docs.ansible.com/ansible/latest/user_guide/playbooks_intro.html#about-playbooks

## Hosts file
hosts file is also known as the inventory file. This file consists of all the information required to connect to other 
nodes to execute Ansible instructions. In this project this file needs to be set in a certain configuration because the 
go code or binary will populate this file automatically with the appropriate information required to connect to local or 
remote containers. 

#### Note: Add as exactly specified below 
```
all:
  vars:
    ansible_python_interpreter: /usr/bin/python3 // Path to your python 3 interpreter 
main:
  hosts:
    host1:
      // Note: These values will be automatically overwritten 
      // by the Go functions 
      ansible_host: 0.0.0.0 
      ansible_port: 39269
      ansible_user: master
      ansible_ssh_pass: password
      ansible_sudo_pass: password
```

## Ports.json 
The ```ports.json``` file is intended to mention the number of ports required 
by the plugin.

```
{
  "NumOfPorts": <number of ports>
}
```

## Description file
This is a simple text file used to describe what the module does. 
When the client is looking at various commands via the ClI.
The description is displayed along-side the plugin name. 

Ex: When the flag ```--ViewPlugins``` or ```--vp``` is called 
```
{
	"PluginsDetected": [
		{
			"FolderName": "<name of the plugin>",
			"PluginDescription": "<description of the plugin>"
		}
	]
} 

```

## Automatic port allocations
P2PRC would be in-charge to set to the ports to various TCP ports 
opened. Due to this implementation the plugin being executed is 
copied to the tmp directory with a unique UUID. 
```
Command: ls /tmp
output: Semantic <UUID>_<Plugin Name> 
2e6d76c4-0ed1-4b55-9385-79a58d4f0492_p2prc-vscode-browser                
7b631e08-62ee-4c1c-a2a4-c05857b9aa7d_p2prc-vscode-browser
```
Once the copy of the plugin is added to the /tmp directory 
the site.yml file inside the appropriate yaml is modified 
with the appropriate ports assigned to the container. 

### Ex: 
1. Create container called c1 with an automatic generated TCP port 
   3313 (external) - 3313 (internal)
2. Assumption of plugin p1 exists. p1 has one server which needs to 
   be mapped to a free open TCP port in container c1. Below shows 
   an implementation of a sample site.yml file. 
```
---
- hosts: all
  tasks:
    - name: start vscode code server
      shell: sh server.sh 0.0.0.0:{{index . 0}}
```
Notice there is the following {{index . 0}}. {{index . 0}} does not belong to 
Ansible but rather is a way to mention where to add the external free port 
of the container. We use the golang [template library](https://pkg.go.dev/text/template) 
to parse and populate the site.yml with the appropriate open ports. An array of ints 
which consists of open free ports are sent to the site.yml. 0 in {{index . 0}} refers 
to the index in the int array passed on. 

After the port is automatically it's ready to run !
```
---
- hosts: all
  tasks:
    - name: start vscode code server
      shell: sh server.sh 0.0.0.0:3313
```

### Sample plugins implemented: 
- [VSCode Plugin](https://github.com/Akilan1999/p2prc-vscode-browser)
