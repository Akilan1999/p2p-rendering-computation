package client

import (
	"encoding/json"
	"fmt"
	"github.com/Akilan1999/p2p-rendering-computation/p2p"
	"github.com/Akilan1999/p2p-rendering-computation/server/docker"
	"io/ioutil"
	"net/http"
	"strconv"
)

var (
	serverPort = "8088"
	client     = http.Client{}
)

// StartContainer Start container using REST api Implementation
// From the selected server IP address
// TODO: Test cases for this function
// Calls URL ex: http://0.0.0.0:8088/startcontainer?ports=0&GPU=false&ContainerName=docker-ubuntu-sshd
func StartContainer(IP string, NumPorts int, GPU bool, ContainerName string, baseImage string) (*docker.DockerVM, error) {
	// Passes URL with number of TCP ports to allocated and to give GPU access to the docker container
	var URL string
	//version := p2p.Ip4or6(IP)
	//
	////Get port number of the server
	//serverPort, err := GetServerPort(IP)
	//if err != nil {
	//	return nil, err
	//}

	//if version == "version 6" {
	//	URL = "http://[" + IP + "]:" + serverPort + "/startcontainer?ports=" + fmt.Sprint(NumPorts) + "&GPU=" + strconv.FormatBool(GPU) + "&ContainerName=" + ContainerName
	//} else {
	URL = "http://" + IP + "/startcontainer?ports=" + fmt.Sprint(NumPorts) + "&GPU=" + strconv.FormatBool(GPU) + "&ContainerName=" + ContainerName + "&BaseImage=" + baseImage
	//}

	resp, err := http.Get(URL)
	if err != nil {
		return nil, err
	}

	// Convert response to byte value
	byteValue, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		return nil, err
	}

	// Create variable for result response type
	var dockerResult docker.DockerVM

	// Adds byte value to docker.DockerVM struct
	json.Unmarshal(byteValue, &dockerResult)
	if err != nil {
		return nil, err
	}

	// Adds the container to the tracked list
	err = AddTrackContainer(&dockerResult, IP)
	if err != nil {
		return nil, err
	}

	return &dockerResult, nil
}

// RemoveContianer Stops and removes container from the server
func RemoveContianer(IP string, ID string) error {
	var URL string
	//version := p2p.Ip4or6(IP)
	//
	////Get port number of the server
	//serverPort, err := GetServerPort(IP)
	//if err != nil {
	//	return err
	//}

	//if version == "version 6" {
	//	URL = "http://[" + IP + "]:" + serverPort + "/RemoveContainer?id=" + ID
	//} else {
	URL = "http://" + IP + "/RemoveContainer?id=" + ID
	//}
	resp, err := http.Get(URL)
	if err != nil {
		return err
	}

	// Convert response to byte value
	byteValue, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		return err
	}

	// Checks if success is returned in the body
	if string(byteValue[:]) == "success" {
		fmt.Println("success")
	}

	// Remove container from groups it exists in
	err = RemoveContainerGroups(ID)
	if err != nil {
		return err
	}
	// Remove container created from the tracked list
	err = RemoveTrackedContainer(ID)
	if err != nil {
		return err
	}

	return nil
}

// ViewContainers This function displays all containers available on server side
func ViewContainers(IP string) (*docker.DockerContainers, error) {
	// Passes URL with route /ShowImages
	var URL string
	//version := p2p.Ip4or6(IP)
	//
	////Get port number of the server
	//serverPort, err := GetServerPort(IP)
	//if err != nil {
	//	return nil, err
	//}

	//if version == "version 6" {
	//	URL = "http://[" + IP + "]:" + serverPort + "/ShowImages"
	//} else {
	URL = "http://" + IP + "/ShowImages"
	//}
	resp, err := http.Get(URL)
	if err != nil {
		return nil, err
	}

	// Convert response to byte value
	byteValue, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		return nil, err
	}

	// Create variable for result response type
	var Result docker.DockerContainers

	// Adds byte value to docker.DockerContainers struct
	json.Unmarshal(byteValue, &Result)
	if err != nil {
		return nil, err
	}

	return &Result, nil
}

// GetServerPort Helper function to do find out server port information
func GetServerPort(IpAddress string) (string, error) {
	// Getting information from the clients ip table
	ipTable, err := p2p.ReadIpTable()
	if err != nil {
		return "", err
	}

	// Iterate thorough ip table struct and find
	// out which port is for the ip address provided
	// in the parameter of the function
	for _, address := range ipTable.IpAddress {
		// If we found a match then return a port
		if address.Ipv4 == IpAddress || address.Ipv6 == IpAddress {
			return address.ServerPort, nil
		}
	}

	// We return default port
	return "8088", nil
}

// PrintStartContainer Prints results Generated container
//func PrintStartContainer(d *docker.DockerVM){
//	fmt.Println("ID : " + fmt.Sprint(d.ID))
//	fmt.Println("SSH port: " + fmt.Sprint(d.SSHPort))
//	fmt.Println("SSH username: " + fmt.Sprint(d.SSHUsername))
//	fmt.Println("SSH password: " + fmt.Sprint(d.SSHPassword))
//	fmt.Println("VNC port: " + fmt.Sprint(d.VNCPort))
//	fmt.Println("VNC password: " + fmt.Sprint(d.VNCPassword))
//	fmt.Println("Ports Open (All TCP ports):")
//	for i := range d.Ports {
//		fmt.Println(d.Ports[i])
//	}
//}

// TODO implementation using RPC calls
//func StartContainer(Ip string) (*docker.DockerVM,error){
//	client, err := rpc.DialHTTP("tcp",Ip + ":" + serverPort)
//
//	if err != nil {
//		return nil,err
//	}
//
//	in := bufio.NewReader(os.Stdin)
//
//	for {
//		line, _, err := in.ReadLine()
//		if err != nil {
//			return nil,err
//		}
//
//		var reply Docker
//
//		err = client.Call("Listener.StartContainer", line, &reply)
//		if err != nil {
//			return nil,err
//		}
//		log.Printf("Reply: %v, Data: %v", reply, reply.docker)
//		return reply.docker, nil
//	}
//
//}
