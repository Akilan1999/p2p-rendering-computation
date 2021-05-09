# Chapter 1: Introduction

## Abstract
This project focuses on creating a framework on running heavy tasks that a regular computer
cannot run easily such as graphically demanding video games, rendering 3D animations , protein
folding simulations. In this project the major focus will not be on the financial incentive part. A peer
to peer network will be created to help run tasks decentrally, increasing bandwidth for running
tasks. To ensure the tasks in the peer to peer network do not corrupt the server 0S (Operating
System), they will be executed in a virtual environment in the server.

The main aim of this project was to create a custom peer to peer network. The user acting as the
client has total flexibility on how to batch the tasks and the user acting as the server has complete
flexibility on tracking the container's usages and killing the containers at any point of time. 

## Motivation
Many of the users rely on our PC / Laptop or servers that belong to a server farm to run heavy
tasks and with the demand of high creativity requires higher computing power. Buying a powerful
computer every few years to run a bunch of heavy tasks which are not executed as frequently to
reap the benefits can be inefficient utilization of hardware. On the other end, renting servers to
run these heavy tasks can be really useful. Ethically speaking this is leading to monopolisation of
computing power similar to what is happening in the web server area. By using peer to peer
principles it is possible to remove the monopolisation factor and increase the bandwidth between
the client and server.

## Aim
This project aims to create a peer to peer (p2p) network, where a user can use the p2p network to
act as a client (i.e sending tasks) or the server (i.e executing the tasks). A prototype application will
be developed, which comes bundled with a p2p module and possible to execute docker containers
or virtual environments across selected nodes.

## Objectives
- Background review on peer to peer network, virtual environments, decentralized
rendering tools and tools to batch any sort of tasks.
- Creating p2p network
- Server to create a containerised environment
- The client node to run tasks on Server containerised node