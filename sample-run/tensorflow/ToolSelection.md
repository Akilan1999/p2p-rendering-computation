# Configuration Management Tool Selection

## Summary

Ansible is proposed as the configuration management tool for this project due to the following characteristics:

- no agent installation on the managed nodes
- push-based configuration changes do not require a separate machine to be provisioned
- commands can be pushed from any developer/operator workstation
- all updates are performed over SSH

## Tools Considered

Where a tool offers an open source and an enterprise version (e.g., Puppet, Terraform), only the open source version was considered. Web or other graphical user interfaces (GUIs) were not considered, as the goal of this phase is to produce infrastructure as code which can be run (and re-run) from the command line interface (CLI).

- [Ansible](#Ansible)
- [Chef](#Chef)
- [Puppet](#Puppet)
- [Salt](#Salt)
- [Terraform](#Terraform)

### Ansible

[Ansible's simple interface and usability fit right into the sys admin mindset, and in a shop with lots of Linux and Unix systems, Ansible is quick and easy to run right out of the gate](https://www.infoworld.com/article/2609482/data-center/data-center-review-puppet-vs-chef-vs-ansible-vs-Salt.html?page=4).

#### Ansible Pros

- Requires no agent installation on the managed nodes.
- Push-based.
- Does not require a separate machine to be provisioned. Commands can be pushed from any developer/operator workstation.
- All updates are performed over SSH.

#### Ansible Cons

- Push-based synchronization means that the configuration is only refreshed on demand.

### Chef

[Chef has a stable and well-designed layout, and while it's not quite up to the level of Puppet in terms of raw features, it's a very capable solution. Chef may pose the most difficult learning curve to administrators who lack significant programming experience, but it could be the most natural fit for development-minded admins and development shops](https://www.infoworld.com/article/2609482/data-center/data-center-review-puppet-vs-chef-vs-ansible-vs-Salt.html?page=4).

#### Chef Pros

- AWS OpsWorks is based on Chef; any future migration to AWS could benefit from the existing configuration files.

#### Chef Cons

- **(Fatal)** Requires a master server and a workstation that will need to be maintained separately from the already provisioned deep learning machines.
- **(Fatal)** Requires an agent (client) to be installed on each machine.
- Requires an on-premises Chef server and [Push Jobs](https://docs.chef.io/push_jobs.html) plugin to push configurations manually. Otherwise, managed machines must check in with the master on a schedule.

### Puppet

[Puppet is the most mature and probably the most approachable of the four from a usability standpoint, though a solid knowledge of Ruby is highly recommended. Puppet is not as streamlined as Ansible or Salt, and its configuration can get Byzantine at times. Puppet is the safest bet for heterogeneous environments, but you may find Ansible or Salt to be a better fit in a larger or more homogenous infrastructure](https://www.infoworld.com/article/2609482/data-center/data-center-review-puppet-vs-chef-vs-ansible-vs-salt.html?page=4).

#### Puppet Pros

- Configuration can be kept up to date via a schedule or manual configuration pushes.

#### Puppet Cons

- **(Fatal)** Requires a "puppet master" (server) that will need to be maintained separately from the already provisioned machines.
- **(Fatal)** Requires an agent (client) to be installed on each machine.

### Salt

[Salt is the sleekest and most robust of the four, and like Ansible it will resonate with sys admins. Highly scalable and quite capable, Salt is hamstrung only by the Web UI](https://www.infoworld.com/article/2609482/data-center/data-center-review-puppet-vs-chef-vs-ansible-vs-salt.html?page=4).

#### Salt Pros

- Push-based.
- Can function via regular SSH or via client agents called "minions".

#### Salt Cons

- When running agentless, push-based synchronization means that the configuration is only refreshed on demand.

### Terraform

The initial hope was that [Terraform](https://terraform.io) could be used for configuration management for this project. However, after reviewing the [available Terraform provider plugins](https://terraform.io/docs/providers) and other available material, it was agreed on 23 January 2019 that Terraform was not suitable for this stage of the project.

In the future, if Arricor migrates its instances to a cloud service provider (CSP) such as AWS, Azure, or Google Cloud Platform, this configuration may be reusable as a first step via the [terraform-provisioner-ansible](https://github.com/jonmorehouse/terraform-provisioner-ansible) plugin. This plugin allows the user to create CSP instances, then execute the given Ansible playbook against those instances.

#### Terraform Pros

- Cloud native with support for multiple service providers.

#### Terraform Cons

- **(Fatal)** No support for provisioning bare metal instances.

## Conclusion

Ansible allows for management of a server from scratch, with only the requirement to have a valid username and password. Additionally, its agentless approach avoids having to configure and maintain an admin server, further simplifying operations at the scale Arricor requires.
