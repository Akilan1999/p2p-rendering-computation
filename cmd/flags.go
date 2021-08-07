package cmd

import (
	"github.com/urfave/cli/v2"
)

// Variables declared for CLI
var (
	AddServer         string
	ViewImages        string
	CreateVM          string
	ContainerName     string
	Ports             string
	Server            bool
	RemoveVM          string
	ID                string
	Specs             string
	GPU               bool
	UpdateServerList  bool
	ServerList        bool
	SetDefaultConfig  bool
	NetworkInterface  bool
	ViewPlugin        bool
	TrackedContainers bool
	ExecutePlugin     string
)

var AppConfigFlags = []cli.Flag{
	// Deprecated to be implemented using GRPC
	&cli.BoolFlag{
		Name:        "Server",
		Aliases: []string{"s"},
		Usage:       "Starts server",
		EnvVars: []string{"SERVER"},
		Destination: &Server,
	},
	&cli.BoolFlag{
		Name:        "UpdateServerList",
		Aliases: []string{"us"},
		Usage:       "Update List of Server available based on servers iptables",
		EnvVars: []string{"UPDATE_SERVER_LIST"},
		Destination: &UpdateServerList,
	},
	&cli.BoolFlag{
		Name:        "ListServers",
		Aliases: []string{"ls"},
		Usage:       "List servers which can render tasks",
		EnvVars: []string{"LIST_SERVERS"},
		Destination: &ServerList,
	},
	&cli.StringFlag{
		Name:        "AddServer",
		Aliases: []string{"as"},
		Usage:       "Adds server IP address to iptables",
		EnvVars: []string{"ADD_SERVER"},
		Destination: &AddServer,
	},
	&cli.StringFlag{
		Name:        "ViewImages",
		Aliases: []string{"vi"},
		Usage:       "View images available on the server IP address",
		EnvVars: []string{"VIEW_IMAGES"},
		Destination: &ViewImages,
	},
	&cli.StringFlag{
		Name:        "CreateVM",
		Aliases: []string{"touch"},
		Usage:       "Creates Docker container on the selected server",
		EnvVars: []string{"CREATE_VM"},
		Destination: &CreateVM,
	},
	&cli.StringFlag{
		Name:        "ContainerName",
		Aliases: []string{"cn"},
		Usage:       "Specifying the container run on the server side",
		EnvVars: []string{"CONTAINER_NAME"},
		Destination: &ContainerName,
	},
	&cli.StringFlag{
		Name:        "RemoveVM",
		Aliases: []string{"rm"},
		Usage:       "Stop and Remove Docker container",
		EnvVars: []string{"REMOVE_VM"},
		Destination: &RemoveVM,
	},
	&cli.StringFlag{
		Name:        "ID",
		Aliases: []string{"id"},
		Usage:       "Docker Container ID",
		EnvVars: []string{"ID"},
		Destination: &ID,
	},
	&cli.StringFlag{
		Name:        "Ports",
		Aliases: []string{"p"},
		Usage:       "Number of ports to open for the Docker Container",
		EnvVars: []string{"NUM_PORTS"},
		Destination: &Ports,
	},
	&cli.BoolFlag{
		Name:        "GPU",
		Aliases: []string{"gpu"},
		Usage:       "Create Docker Containers to access GPU",
		EnvVars: []string{"USE_GPU"},
		Destination: &GPU,
	},
	&cli.StringFlag{
		Name:        "Specification",
		Aliases: []string{"specs"},
		Usage:       "Specs of the server node",
		EnvVars: []string{"SPECS"},
		Destination: &Specs,
	},
	&cli.BoolFlag{
		Name:        "SetDefaultConfig",
		Aliases: []string{"dc"},
		Usage:       "Sets a default configuration file",
		EnvVars: []string{"SET_DEFAULT_CONFIG"},
		Destination: &SetDefaultConfig,
	},
	&cli.BoolFlag{
		Name:        "NetworkInterfaces",
		Aliases: []string{"ni"},
		Usage:       "Shows the network interface in your computer",
		EnvVars: []string{"NETWORK_INTERFACE"},
		Destination: &NetworkInterface,
	},
	&cli.BoolFlag{
		Name:        "ViewPlugins",
		Aliases: []string{"vp"},
		Usage:       "Shows plugins available to be executed",
		EnvVars: []string{"VIEW_PLUGIN"},
		Destination: &ViewPlugin,
	},
	&cli.BoolFlag{
		Name:        "TrackedContainers",
		Aliases: []string{"tc"},
		Usage:       "View containers which have " +
			         "been created from the client side ",
		EnvVars: []string{"TRACKED_CONTAINERS"},
		Destination: &TrackedContainers,
	},
	&cli.StringFlag{
		Name:        "ExecutePlugin",
		Aliases: []string{"plugin"},
		Usage:       "Plugin which needs to be executed",
		EnvVars: []string{"EXECUTE_PLUGIN"},
		Destination: &ExecutePlugin,
	},
}