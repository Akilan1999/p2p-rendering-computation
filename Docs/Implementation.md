# Implementation
| [◀ Previous](Introduction.md) | [Back to TOC](README.md) |
|:-----------:|---------|

This chapter describes how the project was built. It talks in depth of the implementation
performed to give a better understanding of the project.

## Programming langauge used
The programming language used for this project was [Golang](https://go.dev/). The reason Go lang was chosen was
because it is a compiled language.<br>
The entire codebase is just a single binary file. When
distributing to other linux distributing the only requirement would be the binary file to run the
code. It is easy to write independant modules and be monolithic at the sametime using Go.<br>
Using Go.mod makes it very easy to handle external libraries and modularise code. The go.mod name for
the project is [git.sr.ht/~akilan1999/p2p-rendering-computation](https://git.sr.ht/~akilan1999/p2p-rendering-computation).

- ## [Cli Module](CliImplementation.md)
- ## [Config Module](ConfigImplementation.md)
- ## [Server Module](ServerImplementation.md)
- ## [Client Module](ClientImplementation.md)
- ## [P2P Module](P2PImplementation.md)
- ## [Plugin Module](PluginImplementation.md)
- ## [Generate Module](GenerateImplementation.md)