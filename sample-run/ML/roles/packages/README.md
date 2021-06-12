# The `packages` Role

The `packages` role installs the operating system and Python pre-requisites that are required to execute the other roles.

This role may be executed independently by running:

```bash
$ ansible-playbook packages.yml --tags "packages"
```

On execution, this role installs the following directly to the remote machine's operating system:

### Operating System Packages (Debian/Ubuntu)

- apt-transport-https v1.x
- ca-certificates v20180409
- curl v7.x
- gnupg-agent v2.x
- python-apt v1.x
- python-pip v9.x
- python-setuptools v39.x
- software-properties-common v0.96.24.32.7

### Python (pip) Packages

- docker v3.7.0 or greater
