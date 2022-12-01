package frp

import (
    "github.com/fatedier/frp/pkg/config"
    "github.com/fatedier/frp/server"
)

type ServerFRPConf struct {
    address string
}

// StartFRPServer The initial plan is only support reverse proxy
// for TCP ports
// This function starts a server that can act as a reverse
// proxy for nodes behind NAT.
func StartFRPServer(address string, port int) error {
    baseConfig := config.GetDefaultServerConf()
    baseConfig.BindAddr = address

    //TODO look into later for dashboard
    //baseConfig.DashboardAddr = "127.0.0.1"
    //baseConfig.DashboardPort = 8754
    //baseConfig.DashboardUser = "admin"
    //baseConfig.DashboardPwd = "admin"

    // todo change to make it a dynamic port
    baseConfig.BindPort = port
    service, err := server.NewService(baseConfig)
    if err != nil {
        return err
    }

    service.Run()
    return nil
}
