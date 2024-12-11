package frp

import (
	"io/ioutil"
	"net/http"

	"github.com/fatedier/frp/pkg/config"
	"github.com/fatedier/frp/server"
	"github.com/phayes/freeport"
)

type Server struct {
	address string
	port    int
}

// StartFRPProxyFromRandom starts
// reverse proxy server based on
// a random port generated
func StartFRPProxyFromRandom() (int, error) {
	// gets current configuration
	//config, err := configP2PRC.ConfigInit()
	//if err != nil {
	//    return err
	//}

	var s Server
	s.address = "0.0.0.0"

	// use random port
	OpenPorts, err := freeport.GetFreePorts(1)
	if err != nil {
		return 0, err
	}

	s.port = OpenPorts[0]

	// start FRP server

	go s.StartFRPServer()

	return OpenPorts[0], nil

}

// StartFRPServer The initial plan is only support reverse proxy
// for TCP ports
// This function starts a server that can act as a reverse
// proxy for nodes behind NAT.
func (s *Server) StartFRPServer() error {
	baseConfig := config.GetDefaultServerConf()
	baseConfig.BindAddr = s.address

	baseConfig.BindPort = s.port
	service, err := server.NewService(baseConfig)
	if err != nil {
		return err
	}

	service.Run()
	return nil
}

// GetFRPServerPort Gets the port no from the FRPServer to establish
// the FRP connection needed.
func GetFRPServerPort(host string) (string, error) {
	resp, err := http.Get(host + "/FRPPort")

	if err != nil {
		return "", err
	}

	defer resp.Body.Close()

	body, err := ioutil.ReadAll(resp.Body)

	if err != nil {
		return "", err
	}

	return string(body), nil
}

// get free port from proxy server for binding as a remote port
// on the client
func GetFRPServerRemotePort(host string) (string, error) {
	resp, err := http.Get(host + "/FreePort")

	if err != nil {
		return "", err
	}

	defer resp.Body.Close()

	body, err := ioutil.ReadAll(resp.Body)

	if err != nil {
		return "", err
	}

	return string(body), nil
}
