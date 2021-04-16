# Testing P2P network

The objective would be to test the p2p network, and the effectiveness of updating
the ip tables. The objective of would be to give the impression to the client and 
server of a Zero configuration setting. For testing there will be a 
test network set. In the testing scenario all will be client and 
server because the IP table does not store clients IP addresses. At current 
number of hopes would be 3 as default. 

### Test Network Scenario 1
The test network consists of 5 nodes acting as a client and server.
The objective would be to have the entire IP table Updated in each node 
with interacting with only 1 node once. Each node has knowledge of 
one node only. 

### Test Network Scenario 2
The second test network has a scenario of a single peer which all the
other nodes connect too. The scenario being when the other nodes 
connect to the single server they download information about nodes
that have connected to the server node before. 

### Testing Broadcast Module 
For testing the broadcast module 2 types of servers will be 
tested. One with a CPU only , another one with a CPU and GPU.
The expected result being that the appropriate results are 
visible. 

#### Results (CPU and GPU):
```
{
	"Hostname": "akilan-Lenovo-IdeaPad-Y510P",
	"Platform": "ubuntu",
	"CPU": "Intel(R) Core(TM) i7-4700MQ CPU @ 2.40GHz",
	"RAM": 7872,
	"Disk": 937367,
	"GPU": {
		"DriveVersion": "390.141",
		"Gpu": {
			"GpuName": "GeForce GT 755M",
			"BiosVersion": "80.07.A8.00.0F",
			"FanSpeed": "N/A",
			"Utilization": {
				"GpuUsage": "N/A",
				"MemoryUsage": "N/A"
			},
			"Temperature": {
				"GpuTemp": "66 C"
			},
			"Clock": {
				"GpuClock": "N/A",
				"GpuMemClock": "N/A"
			}
		}
	}
} 
```
At the moment of the current implementation v1.0. Nvidia GPU 
are only compatible. As the Go code calls the command ``nvidia-smi``
to get information about the GPU available. 

#### Results (CPU only)
```
{
	"Hostname": "sv-t1.small.x86-01",
	"Platform": "ubuntu",
	"CPU": "Intel(R) Atom(TM) CPU  C2750  @ 2.40GHz",
	"RAM": 7944,
	"Disk": 138793,
	"GPU": null
} 
```
As the ``nvidia-smi`` interface was not detected it only broadcasts
the CPU specs available. 

### Unit tests 
All functions implemented on the P2P module returns type error. 
The units test call certain functions and check if the functions 
return an error or not. This proved sufficient as the point of 
the units tests was code coverage to check if certain functions 
return an error. 

#### Functions tested 
This sections talks about the function called and represents 
code coverage. 

1. ``TestServer_SpeedTest``: Function called LocalSpeedTestIpTable()
2. ``TestReadIpTable``: Function called ReadIpTable()

The P2P module has a 100% code coverage in unit tests as both the unit 
tests call directly or call within the function all the functions used 
in the P2P module. 
