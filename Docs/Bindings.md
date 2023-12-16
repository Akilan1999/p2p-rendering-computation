# Language Bindings
[Language bindings](https://en.wikipedia.org/wiki/Language_binding) refers to wrappers to bridge 2 programming languages. This is used in P2PRC to extend calling P2PRC functions in other programming languages. Currently this is done by generating ```.so``` and ```.h``` from the Go compiler.

## Current languages supported
- Python

## How to build shared object files 
The easier way
```bash
make sharedObjects
```
Or the direct way
```bash
cd Bindings && go build -buildmode=c-shared -o p2prc.so
```
If successfully built:
```bash
# Enter into the Bindings directory
cd Bindings
# List files
ls
# Find files
p2prc.h p2prc.so
```





