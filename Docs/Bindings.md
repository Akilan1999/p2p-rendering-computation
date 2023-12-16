# Language Bindings
[Language bindings](https://en.wikipedia.org/wiki/Language_binding) refers to wrappers to bridge 2 programming languages. This is used in P2PRC to extend calling P2PRC functions in other programming languages. Currently this is done by generating ```.so``` and ```.h``` from the Go compiler.

## How to build shared object files 
The easier way
```bash
# Run
make sharedObjects
```
Or the direct way
```bash
# Run
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

## Current languages supported
- Python

### Build sample python program 
The easier way
```bash
# Run
make python
# Expected ouput
Output is in the Directory Bindings/python/export/
# Run
cd Bindings/python/export/
# list files
ls
# Expected output
SharedObjects/	p2prc.py
```
Above shows a generated folder which consists of a folder 
called "SharedObjects/" which consists of ```p2prc.so```
and ```p2prc.h``` files. ```p2prc.py``` refers to a 
sample python script calling P2PRC go functions.
To start an any project to extend P2PRC with python,
This generated folder can copied and created as a new
git repo for P2PRC extensions scripted or used a reference 
point as proof of concept that P2PRC can be called from 
other programming languages. 




