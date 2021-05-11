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
  "dockerfile": "/<path>/p2p-rendering-computation/server/docker/containers/docker-ubuntu-sshd/",
  "iptable": "/<path>/p2p-rendering-computation/p2p/ip_table.json",
  "speedtestfile": "/<path>/p2p-rendering-computation/p2p/50.bin"
}
```

