package cmd

import (
	"git.sr.ht/~akilan1999/p2p-rendering-computation/server"
	"github.com/urfave/cli/v2"
)

var CliAction = func(ctx *cli.Context) error {
	if Mode == "server" {
		server.Server()
	}

	//Call function to create Docker container
	if IpAddress != "" {

	}

	return nil
}
