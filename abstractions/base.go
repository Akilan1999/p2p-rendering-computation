package abstractions

import (
	Config "github.com/Akilan1999/p2p-rendering-computation/config"
	"github.com/Akilan1999/p2p-rendering-computation/config/generate"
	"github.com/Akilan1999/p2p-rendering-computation/server"
	"github.com/gin-gonic/gin"
	"os"
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

func MapPort(port string) (entireAddres string, mapPort string, err error) {
	entireAddres, mapPort, err = server.MapPort(port)
	return
}
