# Client Module Implementation

This section focuses in depth on how the client module works. To understand the architecture of
the client module refer to section 4.2. The client module is incharge of communicating with
different servers based on the IP addresses provided to the user. The IP addresses are derived
from peer to peer modules. Section 5.6 talks more in depth regarding the peer to peer module.
The objective here is how the client module interacts with peer to peer module and server module.

### Updating the IP table
The client module calls the peer to peer module to get the local IP table initially, Based on the
servers IP addresses available it calls the speedtest function from the peer to peer module to
update IP addresses with information such as latencies, download and upload speeds. Once this is
done the client module does a Rest Api call to the server to download its IP Table. Once the hops are 
done it writes the appropriate results to the Local IP table. Once this is done it prints out the results. 
To derive parameters such as current the public IP address the url “http://ip-api.com/json/” was called. 
This url returns json response of the current public IP address. This feature will be used in the future 
to ensure that the user's current IP address will not be used for a speed test. 
Clients IP table is updated to the server using a form of type multipart.

### Reading server specifications
The client module calls the route /server_specs and reads the json response. If the json response
was successful then it just calls the pretty print function which just prints the json output in the
terminal.

### Client creating and removing container
The client module uses the servers Rest apis to create and delete containers. To create a container
the client requires 3 parameters being the server ip address, the number of the ports the user
wants to open and if the user wants it connected to the GPU or not. The 3 parameters are sent as a
GET request to the server and the server responds with a json file which has information such as
the container ID, ports open , SSH username, SSH password, VNC username and VNC password.
At the moment the username and password are hard coded from the server side for both SSH and
VNC.
To remove a container the client module only requires the server IP address and the container ID.
The client prints the response from the server Rest api.