package cmd

import (
	"github.com/urfave/cli/v2"
)

// Variables declared for CLI
var (
	AddServer            string
	ViewImages           string
	CreateVM             string
	ContainerName        string
	BaseImage            string
	Ports                string
	Server               bool
	RemoveVM             string
	ID                   string
	Specs                string
	GPU                  bool
	UpdateServerList     bool
	ServerList           bool
	SetDefaultConfig     bool
	NetworkInterface     bool
	ViewPlugin           bool
	TrackedContainers    bool
	ExecutePlugin        string
	CreateGroup          bool
	Group                string
	Groups               bool
	RemoveContainerGroup bool
	RemoveGroup          string
	MAPPort              string
	//FRPProxy             bool
	// Generate only allowed in dev release
	// -- REMOVE ON REGULAR RELEASE --
	Generate   string
	Modulename string
	//--------------------------------
	PullPlugin   string
	RemovePlugin string
	AddMetaData  string
	PluginArgs   cli.StringSlice
)

var AppConfigFlags = []cli.Flag{
	// Deprecated to be implemented using GRPC
	&cli.BoolFlag{
		Name:        "Server",
		Aliases:     []string{"s"},
		Usage:       "Starts server",
		EnvVars:     []string{"SERVER"},
		Destination: &Server,
	},
	&cli.BoolFlag{
		Name:        "UpdateServerList",
		Aliases:     []string{"us"},
		Usage:       "Update List of Server available based on servers iptables",
		EnvVars:     []string{"UPDATE_SERVER_LIST"},
		Destination: &UpdateServerList,
	},
	&cli.BoolFlag{
		Name:        "ListServers",
		Aliases:     []string{"ls"},
		Usage:       "List servers which can render tasks",
		EnvVars:     []string{"LIST_SERVERS"},
		Destination: &ServerList,
	},
	&cli.StringFlag{
		Name:        "AddServer",
		Aliases:     []string{"as"},
		Usage:       "Adds server IP address to iptables",
		EnvVars:     []string{"ADD_SERVER"},
		Destination: &AddServer,
	},
	&cli.StringFlag{
		Name:        "ViewImages",
		Aliases:     []string{"vi"},
		Usage:       "View images available on the server IP address",
		EnvVars:     []string{"VIEW_IMAGES"},
		Destination: &ViewImages,
	},
	&cli.StringFlag{
		Name:        "CreateVM",
		Aliases:     []string{"touch"},
		Usage:       "Creates Docker container on the selected server",
		EnvVars:     []string{"CREATE_VM"},
		Destination: &CreateVM,
	},
	&cli.StringFlag{
		Name:        "ContainerName",
		Aliases:     []string{"cn"},
		Usage:       "Specifying the container run on the server side",
		EnvVars:     []string{"CONTAINER_NAME"},
		Destination: &ContainerName,
	},
	&cli.StringFlag{
		Name:        "BaseImage",
		Aliases:     []string{"bi"},
		Usage:       "Specifying the docker base image to template the dockerfile",
		EnvVars:     []string{"CONTAINER_NAME"},
		Destination: &BaseImage,
	},
	&cli.StringFlag{
		Name:        "RemoveVM",
		Aliases:     []string{"rm"},
		Usage:       "Stop and Remove Docker container (IP:port) accompanied by container ID via --ID or --id",
		EnvVars:     []string{"REMOVE_VM"},
		Destination: &RemoveVM,
	},
	&cli.StringFlag{
		Name:        "ID",
		Aliases:     []string{"id"},
		Usage:       "Docker Container ID",
		EnvVars:     []string{"ID"},
		Destination: &ID,
	},
	&cli.StringFlag{
		Name:        "Ports",
		Aliases:     []string{"p"},
		Usage:       "Number of ports to open for the Docker Container",
		EnvVars:     []string{"NUM_PORTS"},
		Destination: &Ports,
	},
	&cli.BoolFlag{
		Name:        "GPU",
		Aliases:     []string{"gpu"},
		Usage:       "Create Docker Containers to access GPU",
		EnvVars:     []string{"USE_GPU"},
		Destination: &GPU,
	},
	&cli.StringFlag{
		Name:        "Specification",
		Aliases:     []string{"specs"},
		Usage:       "Specs of the server node",
		EnvVars:     []string{"SPECS"},
		Destination: &Specs,
	},
	&cli.BoolFlag{
		Name:        "SetDefaultConfig",
		Aliases:     []string{"dc"},
		Usage:       "Sets a default configuration file",
		EnvVars:     []string{"SET_DEFAULT_CONFIG"},
		Destination: &SetDefaultConfig,
	},
	&cli.BoolFlag{
		Name:        "NetworkInterfaces",
		Aliases:     []string{"ni"},
		Usage:       "Shows the network interface in your computer",
		EnvVars:     []string{"NETWORK_INTERFACE"},
		Destination: &NetworkInterface,
	},
	&cli.BoolFlag{
		Name:        "ViewPlugins",
		Aliases:     []string{"vp"},
		Usage:       "Shows plugins available to be executed",
		EnvVars:     []string{"VIEW_PLUGIN"},
		Destination: &ViewPlugin,
	},
	&cli.BoolFlag{
		Name:    "TrackedContainers",
		Aliases: []string{"tc"},
		Usage: "View (currently running) containers which have " +
			"been created from the client side ",
		EnvVars:     []string{"TRACKED_CONTAINERS"},
		Destination: &TrackedContainers,
	},
	&cli.StringFlag{
		Name:        "ExecutePlugin",
		Aliases:     []string{"plugin"},
		Usage:       "Plugin which needs to be executed",
		EnvVars:     []string{"EXECUTE_PLUGIN"},
		Destination: &ExecutePlugin,
	},
	&cli.BoolFlag{
		Name:        "CreateGroup",
		Aliases:     []string{"cgroup"},
		Usage:       "Creates a new group",
		EnvVars:     []string{"CREATE_GROUP"},
		Destination: &CreateGroup,
	},
	&cli.StringFlag{
		Name:        "Group",
		Aliases:     []string{"group"},
		Usage:       "group flag with argument group ID",
		EnvVars:     []string{"GROUP"},
		Destination: &Group,
	},
	&cli.BoolFlag{
		Name:        "Groups",
		Aliases:     []string{"groups"},
		Usage:       "View all groups",
		EnvVars:     []string{"GROUPS"},
		Destination: &Groups,
	},
	&cli.BoolFlag{
		Name:        "RemoveContainerGroup",
		Aliases:     []string{"rmcgroup"},
		Usage:       "Remove specific container in the group",
		EnvVars:     []string{"REMOVE_CONTAINER_GROUP"},
		Destination: &RemoveContainerGroup,
	},
	&cli.StringFlag{
		Name:        "RemoveGroup",
		Aliases:     []string{"rmgroup"},
		Usage:       "Removes the entire group",
		EnvVars:     []string{"REMOVE_GROUP"},
		Destination: &RemoveGroup,
	},
	&cli.StringFlag{
		Name:        "MAPPort",
		Aliases:     []string{"mp"},
		Usage:       "Maps port for a specific port provided as the parameter",
		EnvVars:     []string{"MAPPORT"},
		Destination: &MAPPort,
	},
	// Generate only allowed in dev release
	// -- REMOVE ON REGULAR RELEASE --
	&cli.StringFlag{
		Name:        "Generate",
		Aliases:     []string{"gen"},
		Usage:       "Generates a new copy of P2PRC which can be modified based on your needs",
		EnvVars:     []string{"GENERATE"},
		Destination: &Generate,
	},
	&cli.StringFlag{
		Name:        "ModuleName",
		Aliases:     []string{"mod"},
		Usage:       "New go project module name",
		EnvVars:     []string{"MODULENAME"},
		Destination: &Modulename,
	},
	//&cli.BoolFlag{
	//    Name:        "FRPServerProxy",
	//    Aliases:     []string{"proxy"},
	//    Usage:       "Starts proxy for server",
	//    EnvVars:     []string{"FRPSERVERPROXY"},
	//    Destination: &FRPProxy,
	//},
	//--------------------------------
	&cli.StringFlag{
		Name:        "PullPlugin",
		Aliases:     []string{"pp"},
		Usage:       "Pulls plugin from git repos",
		EnvVars:     []string{"PULLPLUGIN"},
		Destination: &PullPlugin,
	},
	&cli.StringFlag{
		Name:        "RemovePlugin",
		Aliases:     []string{"rp"},
		Usage:       "Removes plugin",
		EnvVars:     []string{"REMOVEPLUGIN"},
		Destination: &RemovePlugin,
	},
	&cli.StringFlag{
		Name:        "AddMetaData",
		Aliases:     []string{"amd"},
		Usage:       "Adds metadata about the current node in the p2p network which is then propagated through the network",
		EnvVars:     []string{"ADDMETADATA"},
		Destination: &AddMetaData,
	},
	&cli.StringSliceFlag{
		Name:        "PluginArgs",
		Aliases:     []string{"pArgs"},
		Usage:       "Args for plugin to be executed",
		EnvVars:     []string{"PLUGINARGS"},
		Destination: &PluginArgs,
	},
}
