Ansible Role: cuDNN
=========

Install [cuDNN](https://developer.nvidia.com/cudnn) on Linux System.

Requirements
------------

- [CUDA Toolkit](https://developer.nvidia.com/cuda-toolkit) installed.
- Download [cuDNN](https://developer.nvidia.com/cudnn) to [`files`](files) folder with your NVIDIA Developer account.

Role Variables
--------------

`cuda_version`: The version of CUDA Toolkit installed. (default: 8.0)

`cudnn_version`: The version cuDNN. (default: v5.1)

Dependencies
------------

None.

Example Playbook
----------------

```yaml
- hosts: gpu
  become: yes

  roles:
    - { role: cudnn, cuda_version: 8.0, cudnn_version: v5.1}
```

License
-------

BSD

Author Information
------------------

This role was created by [Daniel D](https://github.com/djx339).
