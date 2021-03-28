package main

import (
	"git.sr.ht/~akilan1999/p2p-rendering-computation/cmd"
	"github.com/urfave/cli/v2"
	"log"
	"os"
)

// VERSION specifies the version of the platform
var VERSION = "1.0.0"
var mode string

// Varaibles if mode is client
var OS, Pull_location ,Run_script string
var List_servers, Ip_table bool

func main() {

	app := cli.NewApp()
	app.Name = "p2p-rendering-computation"
	app.Usage = "p2p cli application to create and access VMs in other servers"
	app.Version = VERSION
	app.Flags = cmd.AppConfigFlags
	app.Action = cmd.CliAction
	//app := &cli.App{
	//	Name:    "p2p-rendering-computation",
	//	Usage:   "allows you to batch tasks in a peer to peer network",
	//	Version: VERSION,
	//	Flags: []cli.Flag {
	//		&cli.StringFlag{
	//		  Name:        "mode",
	//		  Value:       "client",
	//		  Usage:       "Specifies mode of running",
	//		  Destination: &mode,
	//		},
	//		/* list servers */
    //        &cli.BoolFlag{
	//			Name:        "List-servers",
	//			Usage:       "List servers which can render tasks",
	//			Destination: &List_servers,
	//		},
	//		/* List iptable */
	//		&cli.BoolFlag{
	//			Name:        "Ip-table",
	//			Usage:       "Listing servers that can be connected to",
	//			Destination: &Ip_table,
	//		},
	//
	//	  },
	//	  /*action for all flags*/
	//	  Action: func(c *cli.Context) error {
	//		/* action when certain flags are selected */
	//		/*if Run_script == "None" {
	//			fmt.Println("script not excuted as run script not selected")
	//		}*/
	//
	//		if Ip_table == true || List_servers == true{
	//			err := p2p.PrintIpTable()
	//			if err != nil {
	//				log.Fatal(err)
	//			}
	//		}
	//
	//		// If mode server is selected
	//		if mode == "server"{
	//			server.Server()
	//		}
	//
	//		return nil
	//	  },
	//}

	err := app.Run(os.Args)
	if err != nil {
		log.Fatal(err)
	}
}