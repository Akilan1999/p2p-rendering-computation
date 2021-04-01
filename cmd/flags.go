package cmd

import (
	"github.com/urfave/cli/v2"
)

var Mode,IpAddress string
var List_servers, Ip_table, Abspath bool

var AppConfigFlags = []cli.Flag{
	// Deprecated to be implemented using GRPC
	&cli.StringFlag{
		Name:        "Mode",
		Value:       "client",
		Usage:       "Specifies mode of running",
		EnvVars: []string{"P2P_MODE"},
		Destination: &Mode,
	},
	&cli.BoolFlag{
		Name:        "ListServers",
		Usage:       "List servers which can render tasks",
		EnvVars: []string{"LIST_SERVERS"},
		Destination: &List_servers,
	},
	&cli.StringFlag{
		Name:        "CreateVM",
		Usage:       "Creates Docker container on the selected server",
		EnvVars: []string{"CREATE_VM"},
		Destination: &IpAddress,
	},
	&cli.BoolFlag{
		Name:        "FilePath",
		Usage:       "Testing for absolute path",
		EnvVars: []string{"CREATE_VM"},
		Destination: &Abspath,
	},
}