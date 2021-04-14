package cmd

import (
	"fmt"
	"git.sr.ht/~akilan1999/p2p-rendering-computation/client"
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

	if ServerList {
		p2p.PrintIpTable()
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

		imageRes, err := client.StartContainer(CreateVM,PortsInt,GPU)

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


	return nil
}
