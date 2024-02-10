package abstractions

import (
    "github.com/Akilan1999/p2p-rendering-computation/config"
    "github.com/Akilan1999/p2p-rendering-computation/config/generate"
    "github.com/Akilan1999/p2p-rendering-computation/server"
    "github.com/gin-gonic/gin"
)

// Init Initialises p2prc
func Init(customConfig interface{}) (config *config.Config, err error) {
    // set the config file with default paths
    config, err = generate.SetDefaults("P2PRC", false, customConfig, false)
    if err != nil {
        return
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
