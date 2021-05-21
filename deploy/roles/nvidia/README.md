# The `nvidia` Role

The `nvidia` role installs the Nvidia Docker runtime, enabling access to Nvidia CUDA GPU hardware from within Docker containers.

This role may be executed independently by running:

```bash
$ ansible-playbook packages.yml --tags "nvidia"
```

On execution, this role installs the following directly to the remote machine's operating system:

### Apt Repositories (Debian/Ubuntu)

- libnvidia_container
- nvidia_container_runtime
- nvidia_docker

### Apt GPG Signing Keys (Debian/Ubuntu)

- NVIDIA CORPORATION (Open Source Projects) <cudatools@nvidia.com>

### Operating System Packages (Debian/Ubuntu)

- nvidia-docker2 v2.x

Containers are launched with the Nvidia Docker runtime by passing the `--runtime=nvidia` flag, e.g.,

```bash
$ docker run --rm --runtime=nvidia nvidia/cuda nvidia-smi
```
