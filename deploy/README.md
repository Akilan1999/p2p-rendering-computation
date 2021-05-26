# Ansible p2p-rendering-computation
### Modified from: 

This repository contains an Ansible playbook and instructions to create and manage a single (or many) bare metal deep learning machines. For a description of why Ansible was chosen and what other alternatives were considered, please see [ToolSelection.md](ToolSelection.md)

## Quick Reference

If you've already [installed Ansible](#Installation), you can execute the entire playbook by running:

```bash
$ ansible-playbook packages.yml
```

You can also execute only the pieces you need by passing tags on the command line:

- Install only apt/pip [pre-requisites](roles/packages) to execute the other roles:
  ```bash
  $ ansible-playbook packages.yml --tags "packages"
  ```
- Install [Docker CE](roles/docker):
  ```bash
  $ ansible-playbook packages.yml --tags "docker"
  ```
- Install the [Nvidia CUDA GPU drivers](roles/cuda):
  ```bash
  $ ansible-playbook packages.yml --tags "cuda"
  ```
- Install the [Nvidia Docker Runtime](roles/nvidia):
  ```bash
  $ ansible-playbook packages.yml --tags "nvidia"
  ```

## What's Included

After running the ansible script your machines will be loaded with the following:

1. Docker
2. Nvidia CUDA GPU Drivers
3. Nvidia Docker Runtime
4. TensorFlow GPU Python3 Docker Container
5. JupyterLab

## Using This Repository to Configure Your Environment

1. [Installation](#Installation)
2. [Configuration](#Configuration)
3. [Running](#Running)

---

### Installation

Ansible runs on your local machine and sends commands to the remote (machine learning) machines. You'll need ansible installed locally (not on the machine learning boxes).
For macOS users, the easiest way to install Ansible is via [Homebrew](https://brew.sh/):

```bash
$ brew install ansible
```

If that's not your cup of tea, install Ansible by following the directions for your machine [here](https://docs.ansible.com/ansible/latest/installation_guide/intro_installation.html#installing-the-control-machine).

---

### Configuration

Gather the following:

- SSH key or user credentials for the remote account

  **Note:** Ansible does not expose a channel to allow communication between the user and the ssh process to accept a password manually to decrypt an ssh key when using the ssh connection plugin (which is the default). The use of `ssh-agent` is highly recommended.

- List of servers you wish to manage:
  - hostnames/IP addresses
  - SSH port
  - usernames

Copy [hosts.example] to `/etc/ansible/hosts` (if it does not already exist). Populate the `hosts` file (no extension) with the information about the servers you gathered above.

Confirm that you have populated your Ansible hosts file correctly:

```bash
$ ansible-inventory --list
```

---

### Running

Once you're satisfied that you correctly populated your `hosts` file, update the `- hosts:` line of [tensorflow.yml] to reflect the hosts or groups you want to configure.

Examples:

- Apply against a single host defined as `ml2` in `/etc/ansible/hosts`:
  ```yaml
  - hosts: ml2
  ```
- Apply against a group of hosts defined as `production` in `/etc/ansible/hosts`:
  ```yaml
  - hosts: production
  ```
- Apply against all hosts defined in `/etc/ansible/hosts`:
  ```yaml
  - hosts: all
  ```

Then, when you're ready, run the playbook:

```bash
$ ansible-playbook packages.yml --ask-become-pass
```

**Note:** You must have `sudo` access to run the playbook!

Review the output:

- `[ok]` means no change (this task was already completed)
- `[changed]` means the task successfully ran and the change was applied
- `[unreachable]` means the host could not be reached
- `[failed]` means the task ran but failed to complete

`[ok]` and `[changed]` are successful outcomes. Any `[unreachable]` and `[failed]` outputs should be investigated and resolved.

**Note:** This Ansible playbook is idempotent; once a configuration has been successfully applied, if you apply it again, all actions will report `[ok]`.

## Executing Tensorflow Jobs in Your New Environment

1. Point your browser to http://&lt;hostname&gt;:8888 and login with the password you provided.
2. The `jupyter.volumes.source` folder will be mounted as the `notebooks` folder.
3. Edit and execute your Jupyter notebooks as normal!

### Command Line Access

If you need to drop into a GPU-powered TensorFlow environment, SSH into the remote machine and execute the following:

```bash
$ docker run --runtime=nvidia -it --rm tensorflow/tensorflow:latest-gpu-py3 bash
```

**Note:** You must be a member of the `docker` group or have `sudo` access on the _remote machine_ to execute docker commands.

---

## Additional Files

- [ansible.cfg](ansible.cfg) enables SSH credential forwarding. This is a necessary step during data synchronization, as Ansible delegates those credentials to the master/writer host to push the data folder out to each of the mirrors.
- [Dockerfile](Dockerfile) is used to build the Arricor TensorFlow image. See [Docker.md](Docker.md) for additional details.
- [hosts.example](hosts.example) is an example of the Ansible inventory hosts file saved in `/etc/ansible/hosts`
- [secrets.example.yml](secrets.example.yml) is an example of the expected structure of the `secrets.yml` file
