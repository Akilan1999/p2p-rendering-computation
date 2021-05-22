# The `cuda` Role

The `cuda` role installs the Nvidia CUDA GPU repository and drivers.

This role may be executed independently by running:

```bash
$ ansible-playbook packages.yml --tags "cuda"
```

On execution, this role installs the following directly to the remote machine's operating system:

### Apt Repositories (Debian/Ubuntu)

- Nvidia CUDA

### Apt GPG Signing Keys (Debian/Ubuntu)

- cudatools <cudatools@nvidia.com>

### Operating System Packages (Debian/Ubuntu)

- cuda v10.x
