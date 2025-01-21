# Client Module 
This module is incharge of communicating with the server and receiving the appropriate information back from the server. 

## Functions of the Client Module
<!-- - [Interact with the Server Api](#functions-of-the-client-module) -->
- [Decision maker on how the ip table is created or updated](#decision-maker-on-how-the-ip-table-is-created-or-updated)

## Decision maker on how the IP table is created or updated 
- Does a local speedtest to verify and see if the server IP's in the IP table 
are pingable.
- Tries to ping the servers IP Table addresses.
- If it's pingable then it's added as a new entry in the IP table.
- The following steps occurs in the clients IP table. 
- To ensure that the same servers are not being called to update the IP table. There is 
a temporary list of IP address which have already been called in relation to updating the 
IP table. 
- Based on the current implementation there will 3 hops done to update the IP table.
