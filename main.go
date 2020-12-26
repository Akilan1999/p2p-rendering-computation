package main

import (
	"log"
	"os"
	"github.com/urfave/cli/v2"
	"fmt"
)

// VERSION specifies the version of the platform
var VERSION = "0.0.1"

func main() {
	var mode string

	app := &cli.App{
		Name:    "p2p-redering-computation",
		Usage:   "allows you to batch tasks in a peer to peer network",
		Version: VERSION,
		Flags: []cli.Flag {
			&cli.StringFlag{
			  Name:        "mode",
			  Value:       "client",
			  Usage:       "Specifies mode of running",
			  Destination: &mode,
			},
		  },
		Action: func(c *cli.Context) error {
			/*
			Example usage:
			name := "someone"
			if c.NArg() > 0 {
			  name = c.Args().Get(0)
			}
			if language == "spanish" {
			  fmt.Println("Hola", name)
			} else {
			  fmt.Println("Hello", name)
			}
			return nil 
			*/
			fmt.Println(mode)
			if c.NArg() < 0 {
				fmt.Println(c.Args().Get(0))
			}
			return nil
		  },
	}

	err := app.Run(os.Args)
	if err != nil {
		log.Fatal(err)
	}
}