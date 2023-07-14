package main

import (
    "log"
    "os"

    "github.com/Akilan1999/p2p-rendering-computation/cmd"
    "github.com/urfave/cli/v2"
)

// VERSION specifies the version of the platform
var VERSION = "2.0.0"
var mode string

// Varaibles if mode is client
var OS, Pull_location, Run_script string
var List_servers, Ip_table bool

func main() {
    app := cli.NewApp()
    app.Name = "p2p-rendering-computation"
    app.Usage = "p2p cli application to create and access VMs in other servers"
    app.Version = VERSION
    app.Flags = cmd.AppConfigFlags
    app.Action = cmd.CliAction

    err := app.Run(os.Args)
    if err != nil {
        log.Fatal(err)
    }
}
