package cmd

import (
	"fmt"
	"git.sr.ht/~akilan1999/p2p-rendering-computation/client"
	"git.sr.ht/~akilan1999/p2p-rendering-computation/config"
	"git.sr.ht/~akilan1999/p2p-rendering-computation/p2p"
	"git.sr.ht/~akilan1999/p2p-rendering-computation/server"
	"github.com/urfave/cli/v2"
)

var CliAction = func(ctx *cli.Context) error {
	if Mode == "server" {
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

		p2p.PrintIpTable()
	}

	// Displays the IP table
	if ServerList {
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
		IpAddr.Ipv4 = AddServer

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


	return nil
}
