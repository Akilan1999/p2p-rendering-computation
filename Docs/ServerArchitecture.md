# Server Module Architecture 
The server module takes care of setting and removing the virtualization environment (i.e
containers) for accessing and doing the appropriate computation. It also interacts with the peer to
peer module to update the IP table on the server side. The server module
accesses information regarding CPU and GPU specifications of the machine running the server
module. To do Speed tests the server has routes which allows it to upload and download a 50mb.

![UML diagram of server module](images/servermoduleArch.png)