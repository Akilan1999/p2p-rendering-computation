package cmd

import (
	"fmt"
	"strings"

	"github.com/Akilan1999/p2p-rendering-computation/client"
	"github.com/Akilan1999/p2p-rendering-computation/client/clientIPTable"
	"github.com/Akilan1999/p2p-rendering-computation/config/generate"
	"github.com/Akilan1999/p2p-rendering-computation/p2p"
	"github.com/Akilan1999/p2p-rendering-computation/plugin"
	"github.com/Akilan1999/p2p-rendering-computation/server"
	"github.com/urfave/cli/v2"
)

var CliAction = func(ctx *cli.Context) error {
	if Server {
		_, err := server.Server()
		if err != nil {
			fmt.Print(err)
		}
		//server.Rpc()
		for {

		}
	}

	//Listing servers and also updates IP tables (Default 3 hops)
	if UpdateServerList {
		err := clientIPTable.UpdateIpTableListClient()
		if err != nil {
			fmt.Print(err)
		}
		// Reads from ip table and passes it
		// on to struct print function
		//Servers, err := p2p.ReadIpTable()
		//if err != nil {
		//	return err
		//}
		//client.PrettyPrint(Servers)
		p2p.PrintIpTable()
	}

	// Displays the IP table
	if ServerList {
		// Reads from ip table and passes it
		// on to struct print function
		//Servers, err := p2p.ReadIpTable()
		//if err != nil {
		//	return err
		//}
		p2p.PrintIpTable()
	}

	// Add provided IP to the IP table
	if AddServer != "" {
		res, err := p2p.ReadIpTable()
		if err != nil {
			fmt.Println(err)
		}

		//Create variable of type IpAddress and set IP address
		// to it
		var IpAddr p2p.IpAddress

		//Checking if the address is a ipv4
		// or ipv6 address
		ip4Orip6 := p2p.Ip4or6(AddServer)
		if ip4Orip6 == "version 6" {
			IpAddr.Ipv6 = AddServer
		} else {
			IpAddr.Ipv4 = AddServer
		}

		// If a server port is provided then set it
		if Ports != "" {
			IpAddr.ServerPort = Ports
		} else {
			IpAddr.ServerPort = "8088"
		}
		// Append IP address to variable result which
		// is a list
		res.IpAddress = append(res.IpAddress, IpAddr)
		// Adds the new server IP to the iptable
		res.WriteIpTable()

	}

	// Displays all images available on the server side
	if ViewImages != "" {
		imageRes, err := client.ViewContainers(ViewImages)

		if err != nil {
			fmt.Print(err)
		}
		client.PrettyPrint(imageRes)
	}

	// Function called to stop and remove server from Docker
	if RemoveVM != "" {
		if ID == "" {
			fmt.Println("provide container ID via --ID or --id")
		} else {
			err := client.RemoveContianer(RemoveVM, ID)
			if err != nil {
				fmt.Print(err)
			}
		}
	}

	//Call function to create Docker container
	if CreateVM != "" {

		var PortsInt int

		if Ports != "" {
			// Convert Get Request value to int
			fmt.Sscanf(Ports, "%d", &PortsInt)
		}

		// Calls function to do Api call to start the container on the server side
		imageRes, err := client.StartContainer(CreateVM, PortsInt, GPU, ContainerName, BaseImage)

		if err != nil {
			fmt.Print(err)
		}
		client.PrettyPrint(imageRes)
	}

	//Call if specs flag is called
	if Specs != "" {
		specs, err := client.GetSpecs(Specs)
		if err != nil {
			return err
		}

		// Pretty print
		client.PrettyPrint(specs)
	}

	//Sets default paths to the config file
	if SetDefaultConfig {
		_, err := generate.SetDefaults("P2PRC", false, nil, false)
		if err != nil {
			fmt.Print(err)
		}
	}

	//If the network interface flag is called
	// Then all the network interfaces are displayed
	if NetworkInterface {
		err := p2p.ViewNetworkInterface()
		if err != nil {
			fmt.Print(err)
		}
	}

	// If the view plugin flag is called then display all
	// plugins available
	if ViewPlugin {
		plugins, err := plugin.DetectPlugins()
		if err != nil {
			fmt.Print(err)
		}
		client.PrettyPrint(plugins)
	}

	// If the flag Tracked Container is called or the flag
	// --tc
	if TrackedContainers {
		err, trackedContainers := client.ViewTrackedContainers()
		if err != nil {
			fmt.Print(err)
		}
		client.PrettyPrint(trackedContainers)
	}

	//Executing plugin when the plugin flag is called
	// --plugin
	if ExecutePlugin != "" {
		// To execute plugin requires the container ID or group ID provided when being executed
		if ID != "" {
			// fmt.Println(PluginArgs.Value())
			// fmt.Println(strings.Split(PluginArgs.Value()[0], ","))
			err := plugin.CheckRunPlugin(ExecutePlugin, ID, strings.Split(PluginArgs.Value()[0], ","))
			if err != nil {
				fmt.Println(err)
			} else {
				fmt.Println("Success")
			}
		} else {
			fmt.Println("provide container ID via --ID or --id")
		}

	}

	// Executing function to create new group
	// Creates new group and outputs JSON file
	if CreateGroup {
		group, err := client.CreateGroup()
		if err != nil {
			return err
		}
		client.PrettyPrint(group)
	}

	// Actions to be performed when the
	// group flag is called
	// --group <Group ID>
	if Group != "" {
		// Remove container from group based on group ID provided
		// --rmcgroup --id <contianer id>
		if RemoveContainerGroup && ID != "" {
			group, err := client.RemoveContainerGroup(ID, Group)
			if err != nil {
				fmt.Println(err)
			} else {
				client.PrettyPrint(group)
			}
		} else if ID != "" { // Add container to group based on group ID provided
			// --id <Container ID>
			group, err := client.AddContainerToGroup(ID, Group)
			if err != nil {
				fmt.Println(err)
			} else {
				client.PrettyPrint(group)
			}
		} else { // View all information about current group
			group, err := client.GetGroup(Group)
			if err != nil {
				fmt.Println(err)
			} else {
				client.PrettyPrint(group)
			}
		}
	}

	// Execute function to remove entire group
	// when remove group flag is called
	// --rmgroup
	if RemoveGroup != "" {
		err := client.RemoveGroup(RemoveGroup)
		if err != nil {
			fmt.Println(err)
		} else {
			fmt.Println("Group Removed")
		}
	}

	// Execute Function to view all groups
	if Groups {
		groups, err := client.ReadGroup()
		if err != nil {
			fmt.Println(err)
		} else {
			client.PrettyPrint(groups)
		}
	}

	//--------------------------------

	if PullPlugin != "" {
		_, err := plugin.DownloadPlugin(PullPlugin)
		if err != nil {
			fmt.Println(err)
		} else {
			fmt.Println("Success")
		}
	}

	if RemovePlugin != "" {
		err := plugin.DeletePlugin(RemovePlugin)
		if err != nil {
			fmt.Println(err)
		} else {
			fmt.Println("Success")
		}
	}

	if AddMetaData != "" {
		err := clientIPTable.AddCustomInformationToIPTable(AddMetaData)
		if err != nil {
			fmt.Println(err)
		} else {
			fmt.Println("Success")
		}
	}

	if MAPPort != "" {
		address, err := client.MAPPort(MAPPort, DomainName, RemoteAddress)
		if err != nil {
			return err
		}
		if err != nil {
			fmt.Println(err)
		} else {
			client.PrettyPrint(address)
		}
	}

	return nil
}
