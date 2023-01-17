package frp

import (
	"fmt"
	"github.com/fatedier/frp/pkg/config"
	"github.com/fatedier/frp/server"
	"github.com/phayes/freeport"
	"io/ioutil"
	"net/http"
)

// TODO: Implement a way to
// check if ports are taken

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

	fmt.Println(string(body))

	return string(body), nil
}
