# P2P Module Architecture 

The P2P module (i.e Peer to Peer Module) is responsible for storing the IP table and interacting
with the IP table. In the following implementation of the P2P module ,the IP table stores
information about servers available in the network. The other functionality the P2P module takes
care of is doing the appropriate speed tests to the servers in the IP table. This is for informing the
users about nodes which are close by and nodes which have quicker uploads and downloads
speeds. The module is responsible to ensure that there are no duplicate server IPs in the IP table
and to remove all server IPs which are not pingable.

![UML diagram of P2P module](images/p2pmoduleArch.png)