# Problems in implementation 

### Number of Devices in the network: 
The current implementation has the major flaw which is that 
the server has to be in port 8088 to detected in the public 
network. As we know most personal networks have a single IP 
address. This means we cannot have duplicate ports. A fix can 
be to mention the open port on the IP table file.
(Ex: Possible feild to be added)
```
{
 "ip_address": [
  {
   "ipv4": "localhost",
   "latency": 14981051,
   "download": 8142.122540206258,
   "upload": 3578.766512629995,
   "port": 8088
  }
 ]
}
```

### Broadcast of container specs 
At the moment the container specs are not broadcasted rather 
just the machines specs and this module has yet to be tested 
rigorously. 

### Intergration with GPU
Certain machines have GPUs present in them which provide a 
huge advantage for those who want to do rendering and certain 
sort of computation. The Aim is to only allow it to be compatible
with Nvidia. But an better idea would be to provide compatability
with multiple GPU providers. 

