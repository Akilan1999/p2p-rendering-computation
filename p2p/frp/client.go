package frp

import (
    "git.sr.ht/~akilan1999/p2p-rendering-computation/server/docker"
    "github.com/fatedier/frp/client"
    "github.com/fatedier/frp/pkg/config"
    "math/rand"
    "strconv"
)

// Client This struct stores
// client information with server
// proxy connected
type Client struct {
    Name           string
    Server         *Server
    ClientMappings []ClientMapping
}

// ClientMapping Stores client mapping ports
// to proxy server
type ClientMapping struct {
    LocalIP    string
    LocalPort  int
    RemotePort int
}

// StartFRPClientForServer Starts Server using FRP server
// returns back a port
func StartFRPClientForServer(ipaddress string, port string) (string, error) {
    // Setup server information
    var s Server
    s.address = ipaddress
    // convert port to int
    portInt, err := strconv.Atoi(port)
    if err != nil {
        return "", err
    }
    s.port = portInt

    // Setup client information
    var c Client
    c.Name = "ServerPort"
    c.Server = &s

    //random port
    randPort := rangeIn(10000, 99999)
    c.ClientMappings = []ClientMapping{
        {
            LocalIP:    ipaddress,
            LocalPort:  randPort,
            RemotePort: randPort,
        },
    }

    // Start client server
    go c.StartFRPClient()

    return strconv.Itoa(randPort), nil

}

func StartFRPCDockerContainer(ipaddress string, port string, docker *docker.DockerVM) error {
    // Setup server information
    var s Server
    s.address = ipaddress
    // convert port to int
    portInt, err := strconv.Atoi(port)
    if err != nil {
        return err
    }
    s.port = portInt

    // Setup client information
    var c Client
    c.Name = "ServerPort"
    c.Server = &s

    // set client mapping
    var clientMappings []ClientMapping
    for i, _ := range docker.Ports.PortSet {
        // Set client mapping
        var clientMapping ClientMapping
        portMap := docker.Ports.PortSet[i].ExternalPort
        clientMapping.LocalPort = portMap
        clientMapping.RemotePort = portMap
        // Append to array
        clientMappings = append(clientMappings, clientMapping)
    }

    // Start client server
    go c.StartFRPClient()

    return nil

}

// StartFRPClient Starts FRP client
func (c *Client) StartFRPClient() error {

    cfg := config.GetDefaultClientConf()

    var proxyConfs map[string]config.ProxyConf
    var visitorCfgs map[string]config.VisitorConf

    proxyConfs = make(map[string]config.ProxyConf)

    cfg.ServerAddr = c.Server.address
    cfg.ServerPort = c.Server.port

    for i, _ := range c.ClientMappings {
        var tcpcnf config.TCPProxyConf
        tcpcnf.LocalIP = c.ClientMappings[i].LocalIP
        tcpcnf.LocalPort = c.ClientMappings[i].LocalPort
        tcpcnf.RemotePort = c.ClientMappings[i].RemotePort

        proxyConfs[tcpcnf.ProxyName] = &tcpcnf
    }

    cli, err := client.NewService(cfg, proxyConfs, visitorCfgs, "")
    if err != nil {
        return err
    }

    cli.Run()

    return nil
}

// helper function to generate random
// number in a certain range
func rangeIn(low, hi int) int {
    return low + rand.Intn(hi-low)
}
