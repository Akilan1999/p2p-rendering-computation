# P2P (Peer to Peer module)
In this repository the P2P module has been designed from sratch at the point of this implementation.
[More about function implementation](https://pkg.go.dev/git.sr.ht/~akilan1999/p2p-rendering-computation@v0.0.0-20210404191839-6a046babcb02/p2p)

## Terminology
 1. IPTable: Refers to a json file which stores information about the current servers avaliable with the speedtest results ran from the Node that triggered it. 
 ```
 {
 "ip_address": [
  {
   "ipv4": "localhost",
   "latency": 14981051,
   "download": 8142.122540206258,
   "upload": 3578.766512629995,
  }
 ]
}
 ```


## Responsibility 
- To ensure the IP table has nodes which are pingable 
- Taking to nodes behind NAT. [More about the implementation](NAT-Traversal)...


## Note:
If you are running in server mode it is recommended to use [DMZ](https://routerguide.net/when-and-how-to-setup-dmz-host-for-home-use/) to bypass the [NAT](https://en.wikipedia.org/wiki/Network_address_translation). 
