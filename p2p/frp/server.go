package frp

import (
	"encoding/json"
	"github.com/fatedier/frp/pkg/config"
	"github.com/fatedier/frp/server"
	"github.com/phayes/freeport"
	"io/ioutil"
	"net/http"
	"strconv"
)

type Server struct {
	IPAddress string
	Port      int
	UDPport   int
}

// StartFRPProxyFromRandom starts
// reverse proxy server based on
// a random port generated
func StartFRPProxyFromRandom() (int, int, error) {
	// gets current configuration
	//config, err := configP2PRC.ConfigInit()
	//if err != nil {
	//    return err
	//}

	var s Server
	s.IPAddress = "0.0.0.0"

	// use random port
	OpenPorts, err := freeport.GetFreePorts(2)
	if err != nil {
		return 0, 0, err
	}

	s.Port = OpenPorts[0]
	s.UDPport = OpenPorts[1]

	// start FRP server

	go s.StartFRPServer()

	return s.Port, s.UDPport, nil

}

// StartFRPServer The initial plan is only support reverse proxy
// for TCP and UDP ports
// This function starts a server that can act as a reverse
// proxy for nodes behind NAT.
func (s *Server) StartFRPServer() error {
	baseConfig := config.GetDefaultServerConf()
	baseConfig.BindAddr = s.IPAddress

	baseConfig.BindPort = s.Port
	baseConfig.BindUDPPort = s.UDPport
	service, err := server.NewService(baseConfig)
	if err != nil {
		return err
	}

	service.Run()
	return nil
}

// GetFRPServerPort Gets the port no from the FRPServer to establish
// the FRP connection needed.
// Returns (Opened TCPPort,UDPPort, error)
func GetFRPServerPort(host string) (string, string, error) {
	resp, err := http.Get(host + "/FRPPort")

	if err != nil {
		return "", "", err
	}

	defer resp.Body.Close()

	body, err := ioutil.ReadAll(resp.Body)

	if err != nil {
		return "", "", err
	}

	// we initialize our Users array
	var s Server

	// we unmarshal our byteArray which contains our
	// jsonFile's content into 'users' which we defined above
	json.Unmarshal(body, &s)

	return strconv.Itoa(s.Port), strconv.Itoa(s.UDPport), nil
}
