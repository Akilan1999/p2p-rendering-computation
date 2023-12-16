# Language Bindings
[Language bindings](https://en.wikipedia.org/wiki/Language_binding) refers to wrappers to bridge 2 programming languages. This is used in P2PRC to extend calling P2PRC functions in other programming languages. Currently this is done by generating ```.so``` and ```.h``` from the Go compiler.

<br> 

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
<br>

## Workings under the hood 
Below are a sample set of commands to 
open the bindings implementation.
```
# run
cd Bindings/
# list files
ls 
# search for file
Client.go
```
### In Client go
There a few things to notice which are different from 
your standard Go programs: 

1. We import "C" which means [Cgo](https://pkg.go.dev/cmd/cgo) is required. 
```go
import "C"
```
2. All functions which are required to be called from other programming languages
have comment such as.
```go
//export <function name>

// ------------ Example ----------------
// The function below allows to externally
// to call the P2PRC function to start containers
// in a specific node in the know list of nodes
// in the p2p network.
// Note: the comment "//export StartContainer".

//export StartContainer
func StartContainer(IP string) (output *C.char) {
     container, err := client.StartContainer(IP, 0, false, "", "")
     if err != nil {
         return C.CString(err.Error())
     }
     return ConvertStructToJSONString(container)
 }
```
3. While looking through the file (If 2 files are compared
it is pretty trivial to notice a common structure).
```go
// --------- Example ------------

//export StartContainer
func StartContainer(IP string) (output *C.char) {
     container, err := client.StartContainer(IP, 0, false, "", "")
     if err != nil {
         return C.CString(err.Error())
     }
     return ConvertStructToJSONString(container)
}

//export ViewPlugin
func ViewPlugin() (output *C.char) {
	plugins, err := plugin.DetectPlugins()
	if err != nil {
		return C.CString(err.Error())
	}
	return ConvertStructToJSONString(plugins)
}

```
It is easy to notice that:
- ```ConvertStructToJSONString(<go object>)```: This is a helper function that convert
  a go object to JSON string initially and converts it to ```CString```.
- ```(output *C.char)```: This is the return type for most of the functions.

#### A Pseudo code to refer to the common function implementation shape could be represented as:
```
func <Function name> (output *C.char) {
      <response>,<error> := <P2PRC function name>(<parameters if needed>)
      if <error> != nil {
          return C.CString(<error>.Error())
      }
      return ConvertStructToJSONString(<response>)
}
```


<br>


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




