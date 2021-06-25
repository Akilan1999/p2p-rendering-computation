package client

import (
	"encoding/json"
	"fmt"
	"git.sr.ht/~akilan1999/p2p-rendering-computation/p2p"
	"git.sr.ht/~akilan1999/p2p-rendering-computation/server"
	"io/ioutil"
	"net/http"
)

// GetSpecs Gets Specs from the server such CPU, GPU usage
// and other basic information which helps set a
// cluster of computer
func GetSpecs(IP string)(*server.SysInfo,error) {
	var URL string
	version := p2p.Ip4or6(IP)
	if version == "version 6" {
		URL = "http://[" + IP + "]:" + serverPort + "/server_info"
	} else {
		URL = "http://" + IP + ":" + serverPort + "/server_info"
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
	var serverSpecsResult server.SysInfo

	// Adds byte value to docker.DockerVM struct
	json.Unmarshal(byteValue, &serverSpecsResult)
	if err != nil {
		return nil,err
	}

	return &serverSpecsResult, nil
}

// PrettyPrint print the contents of the obj (
// Reference: https://stackoverflow.com/questions/24512112/how-to-print-struct-variables-in-console
func PrettyPrint(data interface{}) {
	var p []byte
	//    var err := error
	p, err := json.MarshalIndent(data, "", "\t")
	if err != nil {
		fmt.Println(err)
		return
	}
	fmt.Printf("%s \n", p)
}