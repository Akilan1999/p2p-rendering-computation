# Config Implementation

The configuration module is responsible to store basic information of absolute paths of files being
called in the Go code. In a full-fledged Cli the configuration file can be found in the directory
/etc/<project name> and from there points to location such as where the IP table file is located. In
the future implementation the config file will have information such as number of hops and other
parameters to tweak and to improve the effectiveness of the peer to peer network. The
configuration module was implemented using the library Viper. The Viper library automates
features such as searching in default paths to find out if the configuration file is present. If the
configuration file is not present in the default paths then it auto generates the configuration file.
The configurations file can be in any format. In this project the configuration file was generated using 
JSON format. 

```json
{
 "MachineName": "pc-74-120.customer.ask4.lan",
 "IPTable": "/Users/akilan/Documents/p2p-rendering-computation/p2p/iptable/ip_table.json",
 "DockerContainers": "/Users/akilan/Documents/p2p-rendering-computation/server/docker/containers/",
 "DefaultDockerFile": "/Users/akilan/Documents/p2p-rendering-computation/server/docker/containers/docker-ubuntu-sshd/",
 "SpeedTestFile": "/Users/akilan/Documents/p2p-rendering-computation/p2p/50.bin",
 "IPV6Address": "",
 "PluginPath": "/Users/akilan/Documents/p2p-rendering-computation/plugin/deploy",
 "TrackContainersPath": "/Users/akilan/Documents/p2p-rendering-computation/client/trackcontainers/trackcontainers.json",
 "ServerPort": "8088",
 "GroupTrackContainersPath": "/Users/akilan/Documents/p2p-rendering-computation/client/trackcontainers/grouptrackcontainers.json",
 "FRPServerPort": "True",
 "BehindNAT": "True",
 "CustomConfig": null
}
```

