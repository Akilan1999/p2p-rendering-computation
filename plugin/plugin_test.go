package plugin

import (
	"fmt"
	"git.sr.ht/~akilan1999/p2p-rendering-computation/client"
	"git.sr.ht/~akilan1999/p2p-rendering-computation/config"
	"git.sr.ht/~akilan1999/p2p-rendering-computation/server/docker"
	"strconv"
	"testing"
)

// Test if the dummy plugin added is detected
func TestDetectPlugins(t *testing.T) {
	_, err := DetectPlugins()
	if err != nil {
		t.Fail()
	}
}

// Test ensures that the ansible are executed inside local containers
func TestRunPlugin(t *testing.T) {
	var testips []*ExecuteIP
	var testip1,testip2 ExecuteIP

	// Create docker container and get SSH port
	container1 ,err := docker.BuildRunContainer(0,"false","")
	if err != nil {
		fmt.Println(err)
		t.Fail()
	}

	//Test IP 1 configuration
	testip1.IPAddress = "0.0.0.0"
	testip1.SSHPortNo = strconv.Itoa(container1.Ports.PortSet[0].ExternalPort)

	// Create docker container and get SSH port
	container2 ,err := docker.BuildRunContainer(0,"false","")
	if err != nil {
		fmt.Println(err)
		t.Fail()
	}
	//Test IP 2 configuration
	testip2.IPAddress = "0.0.0.0"
	testip2.SSHPortNo = strconv.Itoa(container2.Ports.PortSet[0].ExternalPort)

	testips = append(testips, &testip1)
	testips = append(testips, &testip2)

	_,err = RunPlugin("TestAnsible",testips)
	if err != nil {
		fmt.Println(err)
		t.Fail()
	}

	// Removing container1 after Ansible is executed
	err = docker.StopAndRemoveContainer(container1.ID)
	if err != nil {
		fmt.Println(err)
		t.Fail()
	}

	err = docker.StopAndRemoveContainer(container2.ID)
	if err != nil {
		fmt.Println(err)
		t.Fail()
	}
}

// Test to ensure that the ansible host file is modified to
// the appropriate IP
func TestExecuteIP_ModifyHost(t *testing.T) {
	var plugin Plugin
	var testip ExecuteIP

	// Get plugin path from config file
	Config, err := config.ConfigInit()
	if err != nil {
		fmt.Println(err)
		t.Fail()
	}
	//Set plugin name
	plugin.FolderName = "TestAnsible"

	//Test IP 1 configuration
	testip.IPAddress = "0.0.0.0"
	testip.SSHPortNo = "41289"

	err = testip.ModifyHost(&plugin,Config.PluginPath)
	if err != nil {
		fmt.Println(err)
		t.Fail()
	}
}

// Test to ensure the cli function runs as intended and executes
// the test ansible script
func TestRunPluginCli(t *testing.T) {
	// Create docker container and get SSH port
	container1 ,err := docker.BuildRunContainer(0,"false","")
	if err != nil {
		fmt.Println(err)
		t.Fail()
	}

	// Ensuring created container is the added to the tracked list
	err = client.AddTrackContainer(container1, "0.0.0.0")
	if err != nil {
		fmt.Println(err)
		t.Fail()
	}

	// Running test Ansible script
	err = RunPluginCli("TestAnsible", container1.ID)
	if err != nil {
		fmt.Println(err)
		t.Fail()
	}

	// Removes container information from the tracker IP addresses
	err = client.RemoveTrackedContainer(container1.ID)
	if err != nil {
		fmt.Println(err)
		t.Fail()
	}

	// Removing container1 after Ansible is executed
	err = docker.StopAndRemoveContainer(container1.ID)
	if err != nil {
		fmt.Println(err)
		t.Fail()
	}
}