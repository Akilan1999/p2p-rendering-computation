# Client Module 
This module is incharge of communicating with the server and receiving the appropriate information back from the server. 
Note: To [read more about the functions](https://pkg.go.dev/git.sr.ht/~akilan1999/p2p-rendering-computation@v0.0.0-20210404191839-6a046babcb02/client)

## Functions of the Client Module
- [Interact with the Server Api](#functions-of-the-client-module)
- [Decision maker on how the ip table is created or updated](##decision-maker-on-how-the-ip-table-is-created)


## Interact with the Server Api
This sections talks about the functionality implemented till now. 
- The client can start docker containers using the function StartContainer. 
This functions calls the route:
```
http://<server IP address>:<server port>/startcontainer
```
TODO: Outputs and how it's printed 

## Decision maker on how the IP table is created or updated 
- Does a local speedtest to verify and see if the server IP's in the IP table 
are pingable.
- Downloads the Servers IP table.
- Tries to ping the servers IP Table addresses.
- If it's pingable then it's added as a new entry in the IP table.
- The following steps occurs in the clients IP table. 
- To ensure that the same servers are not being called to update the IP table. There is 
a temporary list of IP address which have already been called in relation to updating the 
IP table. 
- Based on the current implementation there will 3 hops done to update the IP table.
