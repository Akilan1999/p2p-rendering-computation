package plugin

// import (
//     "fmt"
//     "github.com/Akilan1999/p2p-rendering-computation/client"
//     "github.com/Akilan1999/p2p-rendering-computation/config"
//     "github.com/Akilan1999/p2p-rendering-computation/server/docker"
//     "net"
//     "strconv"
//     "testing"
// )
//
// // Test if the dummy plugin added is detected
// func TestDetectPlugins(t *testing.T) {
//     _, err := DetectPlugins()
//     if err != nil {
//         t.Fail()
//     }
// }
//
// // Test ensures that the ansible are executed inside local containers
// func TestRunPlugin(t *testing.T) {
//     var testips []*ExecuteIP
//     var testip1, testip2 ExecuteIP
//
//     // Create docker container and get SSH port
//     container1, err := docker.BuildRunContainer(0, "false", "")
//     if err != nil {
//         fmt.Println(err)
//         t.Fail()
//     }
//
//     //Test IP 1 configuration
//     testip1.IPAddress = "0.0.0.0"
//     testip1.SSHPortNo = strconv.Itoa(container1.Ports.PortSet[0].ExternalPort)
//
//     // Create docker container and get SSH port
//     container2, err := docker.BuildRunContainer(0, "false", "")
//     if err != nil {
//         fmt.Println(err)
//         t.Fail()
//     }
//     //Test IP 2 configuration
//     testip2.IPAddress = "0.0.0.0"
//     testip2.SSHPortNo = strconv.Itoa(container2.Ports.PortSet[0].ExternalPort)
//
//     testips = append(testips, &testip1)
//     testips = append(testips, &testip2)
//
//     _, err = RunPlugin("TestAnsible", testips)
//     if err != nil {
//         fmt.Println(err)
//         t.Fail()
//     }
//
//     // Removing container1 after Ansible is executed
//     err = docker.StopAndRemoveContainer(container1.ID)
//     if err != nil {
//         fmt.Println(err)
//         t.Fail()
//     }
//
//     err = docker.StopAndRemoveContainer(container2.ID)
//     if err != nil {
//         fmt.Println(err)
//         t.Fail()
//     }
// }
//
// // Test to ensure that the ansible host file is modified to
// // the appropriate IP
// func TestExecuteIP_ModifyHost(t *testing.T) {
//     var plugin Plugin
//     var testip ExecuteIP
//
//     // Get plugin path from config file
//     Config, err := config.ConfigInit(nil, nil)
//     if err != nil {
//         fmt.Println(err)
//         t.Fail()
//     }
//     //Set plugin name
//     plugin.FolderName = "TestAnsible"
//     plugin.path = Config.PluginPath
//
//     //Test IP 1 configuration
//     testip.IPAddress = "0.0.0.0"
//     testip.SSHPortNo = "41289"
//
//     err = testip.ModifyHost(&plugin)
//     if err != nil {
//         fmt.Println(err)
//         t.Fail()
//     }
// }
//
// // Test to ensure the cli function runs as intended and executes
// // the test ansible script
// func TestRunPluginContainer(t *testing.T) {
//     // Create docker container and get SSH port
//     container1, err := docker.BuildRunContainer(0, "false", "")
//     if err != nil {
//         fmt.Println(err)
//         t.Fail()
//     }
//
//     // Ensuring created container is the added to the tracked list
//     err = client.AddTrackContainer(container1, "0.0.0.0")
//     if err != nil {
//         fmt.Println(err)
//         t.Fail()
//     }
//
//     // Running test Ansible script
//     err = RunPluginContainer("TestAnsible", container1.ID)
//     if err != nil {
//         fmt.Println(err)
//         t.Fail()
//     }
//
//     // Removes container information from the tracker IP addresses
//     err = client.RemoveTrackedContainer(container1.ID)
//     if err != nil {
//         fmt.Println(err)
//         t.Fail()
//     }
//
//     // Removing container1 after Ansible is executed
//     err = docker.StopAndRemoveContainer(container1.ID)
//     if err != nil {
//         fmt.Println(err)
//         t.Fail()
//     }
// }
//
// // Testing the function can plugin can run with
// // group ID and container ID
// func TestCheckRunPlugin(t *testing.T) {
//     // Create docker container and get SSH port
//     container1, err := docker.BuildRunContainer(0, "false", "")
//     if err != nil {
//         fmt.Println(err)
//         t.Fail()
//     }
//     // Create docker container and get SSH port
//     container2, err := docker.BuildRunContainer(0, "false", "")
//     if err != nil {
//         fmt.Println(err)
//         t.Fail()
//     }
//
//     // Ensuring created container1 is the added to the tracked list
//     err = client.AddTrackContainer(container1, "0.0.0.0")
//     if err != nil {
//         fmt.Println(err)
//         t.Fail()
//     }
//     // Ensuring created container2 is the added to the tracked list
//     err = client.AddTrackContainer(container2, "0.0.0.0")
//     if err != nil {
//         fmt.Println(err)
//         t.Fail()
//     }
//
//     // Create group to add created containers
//     group, err := client.CreateGroup()
//     if err != nil {
//         fmt.Println(err)
//         t.Fail()
//     }
//
//     // Add container 1 to the group
//     _, err = client.AddContainerToGroup(container1.ID, group.ID)
//     if err != nil {
//         fmt.Println(err)
//         t.Fail()
//     }
//
//     // Add container 2 to the group
//     _, err = client.AddContainerToGroup(container2.ID, group.ID)
//     if err != nil {
//         fmt.Println(err)
//         t.Fail()
//     }
//
//     // -------------------------- Main test cases -------------------------------
//
//     // Checking function against container ID
//     err = CheckRunPlugin("TestAnsible", container1.ID)
//     if err != nil {
//         fmt.Println(err)
//         t.Fail()
//     }
//
//     // Checking function against group ID
//     err = CheckRunPlugin("TestAnsible", group.ID)
//     if err != nil {
//         fmt.Println(err)
//         t.Fail()
//     }
//
//     // ----------------------------------------------------------------------------
//
//     // Remove created group
//     err = client.RemoveGroup(group.ID)
//     if err != nil {
//         fmt.Println(err)
//         t.Fail()
//     }
//
//     // Removes container1 information from the tracker IP addresses
//     err = client.RemoveTrackedContainer(container1.ID)
//     if err != nil {
//         fmt.Println(err)
//         t.Fail()
//     }
//
//     // Removing container1 after Ansible is executed
//     err = docker.StopAndRemoveContainer(container1.ID)
//     if err != nil {
//         fmt.Println(err)
//         t.Fail()
//     }
//
//     // Removes container2 information from the tracker IP addresses
//     err = client.RemoveTrackedContainer(container2.ID)
//     if err != nil {
//         fmt.Println(err)
//         t.Fail()
//     }
//
//     // Removing container2 after Ansible is executed
//     err = docker.StopAndRemoveContainer(container2.ID)
//     if err != nil {
//         fmt.Println(err)
//         t.Fail()
//     }
//
// }
//
// func TestDownloadPlugin(t *testing.T) {
//     err := DownloadPlugin("https://github.com/Akilan1999/laplace/")
//     if err != nil {
//
//     }
// }
//
// // Simple test case implemented to the test if
// // the port no can be extracted from the IP address.
// func TestParseIP(t *testing.T) {
//     host, port, err := net.SplitHostPort("12.34.23.13:5432")
//     if err != nil {
//         fmt.Printf("Error: %v\n", err)
//     } else {
//         fmt.Printf("Host: %s\nPort: %s\n", host, port)
//     }
// }
