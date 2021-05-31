ansible-nvidia
====================
ansible roles to install docker-ce, graphics drivers, cuda, cudnn and nvidia-docker

How-to run
----------------
- Install ansible
```
sudo pip3 install ansible
```
- Create hosts file in current directory
```
$ cat hosts
[gpus:vars]
ansible_python_interpreter=/usr/bin/python3

[gpus]
#192.168.1.2
```
- Use locally available ssh keys to authorise logins on a remote machine 
```
ssh-copy-id  root@192.168.1.2
```
- Run playbook
```
ansible-playbook gpus.yml
```
