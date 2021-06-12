# The `docker` Role

The `docker` role installs the Docker CE repository and runtime packages.

This role may be executed independently by running:

```bash
$ ansible-playbook packages.yml --tags "docker"
```

On execution, this role installs the following directly to the remote machine's operating system:

### Apt Repositories (Debian/Ubuntu)

- Docker CE

### Apt GPG Signing Keys (Debian/Ubuntu)

- Docker Release (CE deb) <docker@docker.com>

### Operating System Packages (Debian/Ubuntu)

- docker-ce v5:18.x

### System Services

- `docker` (enabled and started)

### User Groups

- docker

All users listed under `secrets.users` in `secrets.yml` will be added to the `docker` group.
