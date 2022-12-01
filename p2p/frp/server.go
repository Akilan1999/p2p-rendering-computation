package frp

import (
    configP2PRC "git.sr.ht/~akilan1999/p2p-rendering-computation/config"
    "github.com/fatedier/frp/pkg/config"
    "github.com/fatedier/frp/server"
    "strconv"
)

// TODO: Implement a way to
// check if ports are taken

type Server struct {
    address string
    port    int
}

// StartFRPProxyFromConfig starts
// reverse proxy server based on
// the port provided in the config file
// field FRPServerPort.
func StartFRPProxyFromConfig() error {
    // gets current configuration
    config, err := configP2PRC.ConfigInit()
    if err != nil {
        return err
    }

    var s Server
    s.address = "0.0.0.0"
    // Converting config port to int
    port, err := strconv.Atoi(config.FRPServerPort)
    if err != nil {
        return err
    }
    s.port = port

    // start FRP server

    err = s.StartFRPServer()
    if err != nil {
        return err
    }

    return nil

}

// StartFRPServer The initial plan is only support reverse proxy
// for TCP ports
// This function starts a server that can act as a reverse
// proxy for nodes behind NAT.
func (s *Server) StartFRPServer() error {
    baseConfig := config.GetDefaultServerConf()
    baseConfig.BindAddr = s.address

    //TODO look into later for dashboard
    //baseConfig.DashboardAddr = "127.0.0.1"
    //baseConfig.DashboardPort = 8754
    //baseConfig.DashboardUser = "admin"
    //baseConfig.DashboardPwd = "admin"

    // todo change to make it a dynamic port
    baseConfig.BindPort = s.port
    service, err := server.NewService(baseConfig)
    if err != nil {
        return err
    }

    service.Run()
    return nil
}
