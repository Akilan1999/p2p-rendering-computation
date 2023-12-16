> **_NOTE:_**  Fixing documentation to lastest changes ,If you have any questions setting up P2PRC either create an issue or send me an email (me AT akilan dot io).
> Currently HEAD is always intended to stay on a working state. It is recommended to always use HEAD in your go.mod file.
<h1 align="center">
  <br>
  <a href=""><img src="https://raw.githubusercontent.com/Akilan1999/p2p-rendering-computation/master/Docs/images/p2prclogo.png" alt="p2prc" width="400"></a>
  <br>
</h1>

<!-- seperator -->

<div style="display:flex;flex-wrap:wrap;">
  <a href="http://perso.crans.org/besson/LICENSE.html"><img alt="GPLv2 license" src="https://img.shields.io/badge/License-GPLv2-blue.svg" style="padding:5px;margin:5px;" /></a>
  <a href="https://GitHub.com/Akilan1999/p2p-rendering-computation/graphs/commit-activity"><img alt="Maintenance" src="https://img.shields.io/badge/Maintained%3F-yes-green.svg" style="padding:5px;margin:5px;" /></a>
  <a href="http://golang.org"><img alt="made-with-Go" src="https://img.shields.io/badge/Made%20with-Go-1f425f.svg" style="padding:5px;margin:5px;" /></a>
  <a href="https://pkg.go.dev/git.sr.ht/~akilan1999/p2p-rendering-computation"><img alt="GoDoc reference example" src="https://img.shields.io/badge/godoc-reference-blue.svg" style="padding:5px;margin:5px;" /></a>
</div>

The main aim of this project was to create a custom peer to peer network. The user acting as the
client has total flexibility on how to batch the tasks and the user acting as the server has complete
flexibility on tracking the container's usages and killing the containers at any point of time. 

## Latest tutorial
[![IMAGE ALT TEXT](https://i.ytimg.com/vi/OMwCpedu5cs/hqdefault.jpg)](https://www.youtube.com/watch?v=OMwCpedu5cs")

<br>

## Table of contents in the current README 
1. [Introduction](#Introduction)
2. [Installation](#extend-your-application-with-p2prc)
3. [Design Architecture](#Design-Architecture)
4. [Implementation](#Implementation) 
5. [Find out more](#Find-out-more)

<br>

# Table of contents in the Docs folder 
1. [Introduction](Docs/Introduction.md)
2. [Installation](Docs/Installation.md)
3. [Abstractions](Docs/Abstractions.md)
<!-- 3. [Design Architecture](DesignArchtectureIntro.md)
   1. [Client Module](ClientArchitecture.md)
   2. [P2P Module](P2PArchitecture.md)
   3. [Server Module](ServerArchitecture.md) -->
4. [Implementation](Docs/Implementation.md) 
   1. [Client Module](Docs/ClientImplementation.md)
   2. [P2P Module](Docs/P2PImplementation.md)
   3. [Server Module](Docs/ServerImplementation.md)
   4. [Config Module](Docs/ConfigImplementation.md)
   5. [Cli Module](Docs/CliImplementation.md)
   6. [Plugin Module](Docs/PluginImplementation.md)
   7. [Language Bindings](Docs/Bindings.md)
<!-- 5. [Problems](https://github.com/Akilan1999/p2p-rendering-computation/issues)    -->

<br>

## Introduction
This project aims to create a peer to peer (p2p) network, where a user can use the p2p network to act as a client (i.e sending tasks) or the server (i.e executing the tasks). A prototype application will be developed, which comes bundled with a p2p module and possible to execute docker containers or virtual environments across selected nodes.

### Objectives
- Background review on peer to peer network, virtual environments, decentralized rendering tools and tools to batch any sort of tasks.
- Creating p2p network
- Server to create a containerised environment
- The client node to run tasks on Server containerised node

[Read more on the introduction](Docs/Introduction.md)

<br>

## Extend your application with P2PRC 
```go
package main

import (
    "github.com/Akilan1999/p2p-rendering-computation/abstractions"
    "os"
)

func main() {
    // check if the config file exists
    if _, err := os.Stat("config.json"); err != nil {
        // Initialize with base p2prc config files
        _, err := abstractions.Init(nil)
        if err != nil {
            return
        }
    }

    // start p2prc
    _, err := abstractions.Start()
    if err != nil {
        return
    }
}
```

### Export once this is added export P2PRC as environment paths 
```
export P2PRC=<PROJECT PATH>
export PATH=<PROJECT PATH>:${PATH}
```
[Read more](Docs/Abstractions.md) ...

## Installation from source
1. Ensure the Go compiler is installed 
   ```
   go version
   ```
3. Ensure docker is installed (Should run without sudo)
   ```
   docker ps
   ```
3. Clone this repository 
   ```
   git clone https://github.com/Akilan1999/p2p-rendering-computation
   ```
4. Install and build the project
   ```
   make install 
   ```
- If you look closely you will get outputs such as:
   ```
   // Add them to your .bashrc file      
   export P2PRC=/<path>/p2p-rendering-computation
   export PATH=/<path>/p2p-rendering-computation:${PATH}
   ```

5. Test if it works 
   ```
   p2prc -h 
   ```
   or 
   ```
   ./p2prc -h 
   ```
[Read more on the installation and usage](Docs/Installation.md)

<br>

## Design Architecture 
The design architecture was inspired and based on the linux kernel design. The project is segmented into various modules. Each module is responsible for certain tasks in the project. The modules are highly dependent on each other hence the entire codebase can be considered as a huge monolithic chuck which acts as its own library

[Read more on the Design Architecture](Docs/DesignArchtectureIntro.md)

<br>

## Implementation
The programming language used for this project was Golang. The reason Go lang was chosen was because it is a compiled language. The entire codebase is just a single binary file. When distributing to other linux distributing the only requirement would be the binary file to run the code. It is easy to write independant modules and be monolithic at the sametime using Go. Using Go.mod makes it very easy to handle external libraries and modularise code. The go.mod name for the project is git.sr.ht/~akilan1999/p2p-rendering-computation.

[Read more on the Implementation](Docs/Implementation.md)

<br>

## Find out more
As we are working on the open source project p2prc (i.e  p2p network designed for computation).If you are interested in participating as a contributor
or just providing feedback on new features to build  or even just curious about new features added to the project. We have decided to create a discord group.                 
[![Support Server](https://discordapp.com/api/guilds/854397492795277322/widget.png?style=banner2)](https://discord.gg/b4nRGTjYqy)

[![Star History Chart](https://api.star-history.com/svg?repos=Akilan1999/p2p-rendering-computation&type=Date)](https://github.com/Gaurav-Gosain)

