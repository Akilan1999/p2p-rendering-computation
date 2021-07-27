# P2P Module Implementation 
The peer to peer implementation was built from scratch. This is because other peer to peer
libraries were on the implementation of the Distributed hash table. At the current moment all
those heavy features are not needed because the objective is to search and list all possible servers
available. The limitation being that to be a part of the network the user has to know at least 1
server and has to have DMZ enabled from the router if the user wants to act as a server out of the
users local network. The advantage of building from scratch makes the module super light and
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
      "port no": "<server port no>"
    }
  ]
}
```

## Speed Test
The speed test functions populate the fields which are latency, download, upload speed. Before the
speed test begins for each server IP address. The p2p module ensures that each server IP address
is pingable. If the server IP address is not pingable then it removes that IP address from the struct.

### Latency
The latency is measured in milliseconds. The route /server_info is called from the
server and time it takes to provide a json response is recorded.

### Download speed 
The download speed is measured as (<file size>/<time taken to
download>)*8. This gives the result in megabits per second. The file downloaded is a 50 mb
auto generated file.

### Upload speed 
The upload speed is measured as (<file size>/<time taken to upload>)*8. This
gives the results in megabits per second. The file uploaded is a 50 mb auto generated file.
The route /upload is called from the server side to upload the file.
