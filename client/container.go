package client

import (
	"encoding/json"
	"fmt"
	"git.sr.ht/~akilan1999/p2p-rendering-computation/p2p"
	"git.sr.ht/~akilan1999/p2p-rendering-computation/server/docker"
	"io/ioutil"
	"net/http"
	"strconv"
)

const (
	serverPort = "8088"
)


var client = http.Client{}

// StartContainer Start container using REST api Implementation
// From the selected server IP address
// TODO: Test cases for this function
//Calls URL ex: http://0.0.0.0:8088/startcontainer?ports=0&GPU=false&ContainerName=docker-ubuntu-sshd
func StartContainer(IP string, NumPorts int, GPU bool, ContainerName string) (*docker.DockerVM ,error) {
	// Passes URL with number of TCP ports to allocated and to give GPU access to the docker container
	var URL string
	version := p2p.Ip4or6(IP)
	if version == "version 6" {
		URL = "http://[" + IP + "]:" + serverPort + "/startcontainer?ports=" + fmt.Sprint(NumPorts) + "&GPU=" + strconv.FormatBool(GPU) + "&ContainerName=" + ContainerName
	} else {
		URL = "http://" + IP + ":" + serverPort + "/startcontainer?ports=" + fmt.Sprint(NumPorts) + "&GPU=" + strconv.FormatBool(GPU) + "&ContainerName=" + ContainerName
	}

	resp, err := http.Get(URL)
	if err != nil {
		return nil,err
	}

	// Convert response to byte value
	byteValue, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		return nil,err
	}

	// Create variable for result response type
	var dockerResult docker.DockerVM

	// Adds byte value to docker.DockerVM struct
	json.Unmarshal(byteValue, &dockerResult)
	if err != nil {
		return nil,err
	}

	// Adds the container to the tracked list
	err = AddTrackContainer(&dockerResult, IP)
	if err != nil {
		return nil, err
	}

	return &dockerResult, nil
}

// RemoveContianer Stops and removes container from the server
func RemoveContianer(IP string,ID string) error {
	var URL string
	version := p2p.Ip4or6(IP)
	if version == "version 6" {
		URL = "http://[" + IP + "]:" + serverPort + "/RemoveContainer?id=" + ID
	} else {
		URL = "http://" + IP + ":" + serverPort + "/RemoveContainer?id=" + ID
	}
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

    // Remove container created from the tracked list
	err = RemoveTrackedContainer(ID)
	if err != nil {
		return err
	}

	return nil
}

// ViewContainers This function displays all containers available on server side
func ViewContainers(IP string)(*docker.DockerContainers, error){
	// Passes URL with route /ShowImages
	var URL string
	version := p2p.Ip4or6(IP)
	if version == "version 6" {
		URL = "http://[" + IP + "]:" + serverPort + "/ShowImages"
	} else {
		URL = "http://" + IP + ":" + serverPort + "/ShowImages"
	}
	resp, err := http.Get(URL)
	if err != nil {
		return nil,err
	}

	// Convert response to byte value
	byteValue, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		return nil,err
	}

	// Create variable for result response type
	var Result docker.DockerContainers

	// Adds byte value to docker.DockerContainers struct
	json.Unmarshal(byteValue, &Result)
	if err != nil {
		return nil,err
	}

	return &Result, nil
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
