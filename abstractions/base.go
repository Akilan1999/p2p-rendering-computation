package abstractions

// import "C"
import (
	"os"

	"github.com/Akilan1999/p2p-rendering-computation/client"
	"github.com/Akilan1999/p2p-rendering-computation/client/clientIPTable"
	Config "github.com/Akilan1999/p2p-rendering-computation/config"
	"github.com/Akilan1999/p2p-rendering-computation/config/generate"
	"github.com/Akilan1999/p2p-rendering-computation/p2p"
	"github.com/Akilan1999/p2p-rendering-computation/plugin"
	"github.com/Akilan1999/p2p-rendering-computation/server"
	"github.com/Akilan1999/p2p-rendering-computation/server/docker"
	"github.com/gin-gonic/gin"
)

// Init Initialises p2prc
func Init(customConfig interface{}) (config *Config.Config, err error) {

	// Get config file path
	// Checks P2PRC path initially
	// - Get PATH if environment varaible
	path, err := Config.GetPathP2PRC("P2PRC")
	if err != nil {
		return
	}
	// check if the config file exists
	if _, err = os.Stat(path + "config.json"); err != nil {
		// Initialize with base p2prc config files
		// set the config file with default paths
		config, err = generate.SetDefaults("P2PRC", false, customConfig, false)
		if err != nil {
			return
		}
	} else {
		// If the configs are available then use them over generating new ones.
		config, err = Config.ConfigInit(nil, nil)
		if err != nil {
			return
		}
	}

	return
}

// Start p2prc in a server mode
func Start() (*gin.Engine, error) {
	engine, err := server.Server()
	if err != nil {
		return nil, err
	}
	return engine, nil
}

// MapPort Creates a reverse proxy connection and maps the appropriate port
func MapPort(port string) (entireAddres string, mapPort string, err error) {
	entireAddres, mapPort, err = server.MapPort(port)
	return
}

// StartContainer Starts docker container on the remote machine
func StartContainer(IP string, NumPorts int, ContainerName string, BaseImage string) (container *docker.DockerVM, err error) {
	container, err = client.StartContainer(IP, NumPorts, false, ContainerName, BaseImage)
	return
}

// RemoveContainer Removes docker container based on the IP address and ID
// provided
func RemoveContainer(IP string, ID string) error {
	return client.RemoveContianer(IP, ID)
}

// GetSpecs Get spec information about the remote server
func GetSpecs(IP string) (specs *server.SysInfo, err error) {
	specs, err = client.GetSpecs(IP)
	return
}

// ViewIPTable View information of nodes in the network
func ViewIPTable() (table *p2p.IpAddresses, err error) {
	table, err = p2p.ReadIpTable()
	return
}

// UpdateIPTable Force updates IP tables based on new
// new nodes discovered in the network
func UpdateIPTable() (err error) {
	return clientIPTable.UpdateIpTableListClient()
}

func ExecutePlugin(PluginName string, ContainerID string, PluginArgs []string) error {
	return plugin.CheckRunPlugin(PluginName, ContainerID, PluginArgs)
}
