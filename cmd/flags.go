package cmd

import (
	"github.com/urfave/cli/v2"
)

// Variables declared for CLI
var (
	CreateVM    string
	Ports       string
	Mode        string
	RemoveVM    string
	ID          string
	Specs       string
	GPU         bool
	ListServers bool
)

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
		Destination: &ListServers,
	},
	&cli.StringFlag{
		Name:        "CreateVM",
		Usage:       "Creates Docker container on the selected server",
		EnvVars: []string{"CREATE_VM"},
		Destination: &CreateVM,
	},
	&cli.StringFlag{
		Name:        "RemoveVM",
		Usage:       "Stop and Remove Docker container",
		EnvVars: []string{"REMOVE_VM"},
		Destination: &RemoveVM,
	},
	&cli.StringFlag{
		Name:        "ID",
		Usage:       "Docker Container ID",
		EnvVars: []string{"ID"},
		Destination: &ID,
	},
	&cli.StringFlag{
		Name:        "Ports",
		Usage:       "Number of ports to open for the Docker Container",
		EnvVars: []string{"NUM_PORTS"},
		Destination: &ID,
	},
	&cli.BoolFlag{
		Name:        "GPU",
		Usage:       "Create Docker Containers to access GPU",
		EnvVars: []string{"USE_GPU"},
		Destination: &GPU,
	},
	&cli.StringFlag{
		Name:        "Specs",
		Usage:       "Specs of the server node",
		EnvVars: []string{"SPECS"},
		Destination: &Specs,
	},
}