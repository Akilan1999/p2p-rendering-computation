package cmd

import (
	"fmt"
	"git.sr.ht/~akilan1999/p2p-rendering-computation/client"
	"git.sr.ht/~akilan1999/p2p-rendering-computation/config"
	"git.sr.ht/~akilan1999/p2p-rendering-computation/p2p"
	"git.sr.ht/~akilan1999/p2p-rendering-computation/plugin"
	"git.sr.ht/~akilan1999/p2p-rendering-computation/server"
	"github.com/urfave/cli/v2"
)

var CliAction = func(ctx *cli.Context) error {
	if Server {
		// TODO: with RPC calls
		server.Server()
		//server.Rpc()
	}

	//Listing servers and also updates IP tables (Default 3 hops)
	if UpdateServerList {
		err := client.UpdateIpTableListClient()
		if err != nil {
			fmt.Print(err)
		}
		// Reads from ip table and passes it
		// on to struct print function
		Servers, err := p2p.ReadIpTable()
		if err != nil {
			return err
		}
		client.PrettyPrint(Servers)
	}

	// Displays the IP table
	if ServerList {
		// Reads from ip table and passes it
		// on to struct print function
		Servers, err := p2p.ReadIpTable()
		if err != nil {
			return err
		}
		client.PrettyPrint(Servers)
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
	if RemoveVM != ""  && ID != "" {
		err := client.RemoveContianer(RemoveVM,ID)
		if err != nil {
			fmt.Print(err)
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
		imageRes, err := client.StartContainer(CreateVM,PortsInt,GPU,ContainerName)

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
		err := config.SetDefaults()
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
		plugins ,err := plugin.DetectPlugins()
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


	return nil
}
