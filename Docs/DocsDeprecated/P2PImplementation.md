# P2P Module Implementation 

The P2P module (i.e Peer to Peer Module) is responsible for storing the IP table and interacting
with the IP table. In the following implementation of the P2P module ,the IP table stores
information about servers available in the network. The other functionality the P2P module takes
care of is doing the appropriate speed tests to the servers in the IP table. This is for informing the
users about nodes which are close by and nodes which have quicker uploads and downloads
speeds. The module is responsible to ensure that there are no duplicate server IPs in the IP table
and to remove all server IPs which are not pingable.

![UML diagram of P2P module](images/p2pmoduleArch.png)

The peer to peer implementation was built from scratch. This is because other peer to peer
libraries were on the implementation of the Distributed hash table. At the current moment all
those heavy features are not needed because the objective is to search and list all possible servers
available. The limitation being that to be a part of the network the user has to know at least 1
server. The advantage of building from scratch makes the module super light and
possibility for custom functions and structs. The sub topics below will mention the
implementations of each functionality in depth.

## IP Table 
The ip table file is a json as the format with a list of servers ip addresses, latencies, downloads and
uploads speeds. The functions implemented include read
file, write file and remove duplicate IP addresses. The remove duplicate IP address function exists
because sometimes servers IP tables can have the same ip addresses as what the client has. The
path of the IP table json file is received from the configuration module.

```json
{
  "ip_address": [
    {
      "ipv4": "<ipv4 address>",
      "latency": "<latency>",
      "download": "<download>",
      "upload": "<upload>"
      "port no": "<server port no>",
    }
  ]
}
```

### Latency
The latency is measured in milliseconds. The route /server_info is called from the
server and time it takes to provide a json response is recorded.

## NAT Traversal
P2PRC currently supports TURN for NAT traversal. 



## TURN 
The current TURN implementation used is FRP. The TURN server is also required when 
a P2PRC node is acting as a Server. The TURN server is determined based on the Node 
with the least amount of latency based on the Nodes available on the IPTable. 
Once a TURN server is determined there are 2 actions performed. The first one is 
```/FRPPort``` to the TURN server to receive a port which is used to generate the external 
port from the TURN server. The flow below describes the workflow.

### Client mode
- Call ```/FRPPort```
```
http://<turn server ip>:<server port no>/FRPport
```
- Call the TURN server in the following manner. The following is a sample code snippet below. 
```go
import (
    "github.com/Akilan1999/p2p-rendering-computation/p2p/frp"
)

func main() {
  serverPort, err := frp.GetFRPServerPort("http://" + <lowestLatencyIpAddress.Ipv4> + ":" + lowestLatencyIpAddress.ServerPort)
   if err != nil {
    return nil, err
   }
   // Create 1 second delay to allow FRP server to start
   time.Sleep(1 * time.Second)
   // Starts FRP as a client with
   proxyPort, err := frp.StartFRPClientForServer(<lowestLatencyIpAddress.Ipv4>, serverPort, <the port you want to expose externally>)
   if err != nil {
     return nil, err
   }
}
```


