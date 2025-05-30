<!-- Code generated by gomarkdoc. DO NOT EDIT -->

# frp

```go
import "github.com/Akilan1999/p2p-rendering-computation/p2p/frp"
```

## Index

- [func GetFRPServerPort\(host string\) \(string, error\)](<#GetFRPServerPort>)
- [func StartFRPCDockerContainer\(ipaddress string, port string, Docker \*docker.DockerVM\) \(\*docker.DockerVM, error\)](<#StartFRPCDockerContainer>)
- [func StartFRPClientForServer\(ipaddress string, port string, localport string, remoteport string\) \(string, error\)](<#StartFRPClientForServer>)
- [func StartFRPProxyFromRandom\(\) \(int, error\)](<#StartFRPProxyFromRandom>)
- [type Client](<#Client>)
  - [func \(c \*Client\) StartFRPClient\(\) error](<#Client.StartFRPClient>)
- [type ClientMapping](<#ClientMapping>)
- [type Server](<#Server>)
  - [func \(s \*Server\) StartFRPServer\(\) error](<#Server.StartFRPServer>)


<a name="GetFRPServerPort"></a>
## func [GetFRPServerPort](<https://github.com/Akilan1999/p2p-rendering-computation/blob/master/p2p/frp/server.go#L65>)

```go
func GetFRPServerPort(host string) (string, error)
```

GetFRPServerPort Gets the port no from the FRPServer to establish the FRP connection needed.

<a name="StartFRPCDockerContainer"></a>
## func [StartFRPCDockerContainer](<https://github.com/Akilan1999/p2p-rendering-computation/blob/master/p2p/frp/client.go#L90>)

```go
func StartFRPCDockerContainer(ipaddress string, port string, Docker *docker.DockerVM) (*docker.DockerVM, error)
```



<a name="StartFRPClientForServer"></a>
## func [StartFRPClientForServer](<https://github.com/Akilan1999/p2p-rendering-computation/blob/master/p2p/frp/client.go#L35>)

```go
func StartFRPClientForServer(ipaddress string, port string, localport string, remoteport string) (string, error)
```

StartFRPClientForServer Starts Server using FRP server returns back a port remote port is a custom external port a user would want to open. This under the assumption the user knows the exact port available in server doing the TURN connection.

<a name="StartFRPProxyFromRandom"></a>
## func [StartFRPProxyFromRandom](<https://github.com/Akilan1999/p2p-rendering-computation/blob/master/p2p/frp/server.go#L19>)

```go
func StartFRPProxyFromRandom() (int, error)
```

StartFRPProxyFromRandom starts reverse proxy server based on a random port generated

<a name="Client"></a>
## type [Client](<https://github.com/Akilan1999/p2p-rendering-computation/blob/master/p2p/frp/client.go#L16-L20>)

Client This struct stores client information with server proxy connected

```go
type Client struct {
    Name           string
    Server         *Server
    ClientMappings []ClientMapping
}
```

<a name="Client.StartFRPClient"></a>
### func \(\*Client\) [StartFRPClient](<https://github.com/Akilan1999/p2p-rendering-computation/blob/master/p2p/frp/client.go#L143>)

```go
func (c *Client) StartFRPClient() error
```

StartFRPClient Starts FRP client

<a name="ClientMapping"></a>
## type [ClientMapping](<https://github.com/Akilan1999/p2p-rendering-computation/blob/master/p2p/frp/client.go#L24-L28>)

ClientMapping Stores client mapping ports to proxy server

```go
type ClientMapping struct {
    LocalIP    string
    LocalPort  int
    RemotePort int
}
```

<a name="Server"></a>
## type [Server](<https://github.com/Akilan1999/p2p-rendering-computation/blob/master/p2p/frp/server.go#L11-L14>)



```go
type Server struct {
    // contains filtered or unexported fields
}
```

<a name="Server.StartFRPServer"></a>
### func \(\*Server\) [StartFRPServer](<https://github.com/Akilan1999/p2p-rendering-computation/blob/master/p2p/frp/server.go#L49>)

```go
func (s *Server) StartFRPServer() error
```

StartFRPServer The initial plan is only support reverse proxy for TCP ports This function starts a server that can act as a reverse proxy for nodes behind NAT.

Generated by [gomarkdoc](<https://github.com/princjef/gomarkdoc>)
