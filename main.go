package main

import (
	"log"
	"os"
	"github.com/urfave/cli/v2"
	//"fmt"
	"git.sr.ht/~akilan1999/p2p-rendering-computation/server"
)

// VERSION specifies the version of the platform
var VERSION = "0.0.1"
var mode string

// Varaibles if mode is client
var OS, Pull_location ,Run_script string
var List_servers bool 

func main() {

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
			/* 
			Flags if the client mode is selected:
			------------------------------------------------------------------
			  (run task)
			  OS: Operating system name to create the VM 
			  Pull Location: Location to pull the OS image
			  run script: Script to run the task (i.e binary file preferred)
            ------------------------------------------------------------------ 
			*/
			&cli.StringFlag{
				Name:        "OS",
				Value:       "ubuntu",
				Usage:       "Operating system name to create the VM",
				Destination: &OS,
			},
			&cli.StringFlag{
				Name:        "Pull_location",
				Value:       "http://releases.ubuntu.com/14.04.3/ubuntu-14.04.3-server-amd64.iso",
				Usage:       "Location to pull the OS image",
				Destination: &Pull_location,
			},
			&cli.StringFlag{
				Name:        "Run_script",
				Value:       "None",
				Usage:       "Script to run the task (i.e binary file preferred)",
				Destination: &Run_script,
			},
			/* list servers */
            &cli.BoolFlag{
				Name:        "List_servers",
				Usage:       "List servers which can render tasks",
				Destination: &List_servers,
			},

		  },
		  /*action for all flags*/
		  Action: func(c *cli.Context) error {
			/* action when certain flags are selected */
			/*if Run_script == "None" {
				fmt.Println("script not excuted as run script not selected")
			}*/
			
			/*if List_servers == true{ 
				server.Servers() 
			}*/
			
			// If mode server is selected
			if mode == "server"{
				server.Server()
			}

			return nil
		  },
	}

	err := app.Run(os.Args)
	if err != nil {
		log.Fatal(err)
	}
}