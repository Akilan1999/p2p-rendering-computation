package plugin

import (
	"fmt"
	"git.sr.ht/~akilan1999/p2p-rendering-computation/config"
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

	//Test IP 1 configuration
	testip1.IPAddress = "0.0.0.0"
	testip1.SSHPortNo = "33383"

	//Test IP 2 configuration
	testip2.IPAddress = "0.0.0.0"
	testip2.SSHPortNo = "34447"

	testips = append(testips, &testip1)
	testips = append(testips, &testip2)

	_,err := RunPlugin("TestAnsible",testips)
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