package main

import (
	"log"
	"os"
	"github.com/urfave/cli/v2"
)

// VERSION specifies the version of the platform
var VERSION = "0.0.1"

func main() {
	app := &cli.App{
		Name:    "p2p-redering-computation",
		Usage:   "allows you to batch tasks in a peer to peer network",
		Version: VERSION,
		/*Commands: []*cli.Command{
			cmd.CmdStart,
		},*/
	}

	err := app.Run(os.Args)
	if err != nil {
		log.Fatal(err)
	}
}