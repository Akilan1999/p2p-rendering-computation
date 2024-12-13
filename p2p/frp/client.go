package frp

import (
	"math/rand"
	"strconv"
	"time"

	"github.com/Akilan1999/p2p-rendering-computation/server/docker"
	"github.com/fatedier/frp/client"
	"github.com/fatedier/frp/pkg/config"
	"github.com/phayes/freeport"
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
// remote port is a custom external port a user would want
// to open. This under the assumption the user knows the
// exact port available in server doing the TURN connection.
func StartFRPClientForServer(ipaddress string, port string, localport string, remoteport string) (string, error) {
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

	// converts localport to int
	portInt, err = strconv.Atoi(localport)
	if err != nil {
		return "", err
	}

	var OpenPorts []int
	// if the remote port is
	// not empty then set the remote port to that.
	if remoteport != "" {
		// converts localport to int
		portIntRemote, err := strconv.Atoi(remoteport)
		if err != nil {
			return "", err
		}
		OpenPorts = append(OpenPorts, portIntRemote)
	} else {
		//random port
		//randPort := rangeIn(10000, 99999)
		OpenPorts, err = freeport.GetFreePorts(1)

		// portStrRemote, err := GetFRPServerRemotePort("http://" + ipaddress + ":" + port)

		if err != nil {
			return "", err
		}

		// portIntRemote, err := strconv.Atoi(portStrRemote)

		// if err != nil {
		// 	return "", err
		// }

		// OpenPorts = append(OpenPorts, portIntRemote)

	}
	c.ClientMappings = []ClientMapping{
		{
			LocalIP:    "localhost",
			LocalPort:  portInt,
			RemotePort: OpenPorts[0],
		},
	}

	// Start client server
	go c.StartFRPClient()

	return strconv.Itoa(OpenPorts[0]), nil

}

func StartFRPCDockerContainer(ipaddress string, port string, Docker *docker.DockerVM) (*docker.DockerVM, error) {
	// setting new docker variable

	//var DockerFRP docker.DockerVM

	//DockerFRP = *Docker
	//DockerFRP.Ports.PortSet = []docker.Port{}
	// Setup server information
	var s Server
	s.address = ipaddress
	// convert port to int
	portInt, err := strconv.Atoi(port)
	if err != nil {
		return nil, err
	}
	s.port = portInt

	// Setup client information
	var c Client
	c.Name = "ServerPort"
	c.Server = &s

	// set client mapping
	//var clientMappings []ClientMapping
	for i, _ := range Docker.Ports.PortSet {
		portMap := Docker.Ports.PortSet[i].ExternalPort

		serverPort, err := GetFRPServerPort("http://" + ipaddress + ":" + port)
		if err != nil {
			return nil, err
		}

		//delay to allow the FRP server to start
		time.Sleep(1 * time.Second)

		remotePort, err := GetFRPServerRemotePort("http://" + ipaddress + ":" + port)
		if err != nil {
			return nil, err
		}

		proxyPort, err := StartFRPClientForServer(ipaddress, serverPort, strconv.Itoa(portMap), remotePort)
		if err != nil {
			return nil, err
		}

		portInt, err = strconv.Atoi(proxyPort)
		if err != nil {
			return nil, err
		}

		Docker.Ports.PortSet[i].ExternalPort = portInt
	}

	return Docker, nil

}

// StartFRPClient Starts FRP client
func (c *Client) StartFRPClient() error {

	cfg := config.GetDefaultClientConf()

	//Config, err := defaultConfig.ConfigInit(nil, nil)
	//if err != nil {
	//	return err
	//}

	var proxyConfs map[string]config.ProxyConf
	var visitorCfgs map[string]config.VisitorConf

	proxyConfs = make(map[string]config.ProxyConf)

	cfg.ServerAddr = c.Server.address
	cfg.ServerPort = c.Server.port
	//cfg.TLSEnable = true
	//cfg.TLSKeyFile = Config.KeyFile
	//cfg.TLSCertFile = Config.PemFile

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
