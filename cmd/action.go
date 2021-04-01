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

	//Listing servers avaliable
	if List_servers {
		p2p.PrintIpTable()
	}

	//Call function to create Docker container
	if IpAddress != "" {
		imageRes, err := client.StartContainer(IpAddress)

		if err != nil {
			fmt.Print(err)
		}
		client.PrintStartContainer(imageRes)
	}


	return nil
}
