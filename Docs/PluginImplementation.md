# Plugin Module Implementation 
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
                |___ description.txt 
              .
              . 
              .
              n: n number of plugins possible 
```

### Site File Template (i.e site.yml)
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

### Hosts file (i.e hosts) 
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

### Description file (i.e description.txt)
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