package client

import (
	"github.com/Akilan1999/p2p-rendering-computation/config"
	"io/ioutil"
	"net"
	"net/http"
	"strconv"
)

type ResponseMAPPort struct {
	IPAddress     string
	PortNo        string
	EntireAddress string
}

func MAPPort(port string, domainName string, ServerAddress string, udp bool) (*ResponseMAPPort, error) {
	Config, err := config.ConfigInit(nil, nil)
	if err != nil {
		return nil, err
	}

	URL := "http://0.0.0.0:" + Config.ServerPort

	if ServerAddress != "" {
		URL = "http://" + ServerAddress
	}

	//if version == "version 6" {
	URL = URL + "/MAPPort?port=" + port + "&domain_name=" + domainName + "&udp=" + strconv.FormatBool(udp)
	//} else {
	//	URL = "http://" + IP + ":" + serverPort + "/server_info"
	//}
	resp, err := http.Get(URL)
	if err != nil {
		return nil, err
	}

	// Convert response to byte value
	byteValue, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		return nil, err
	}

	host, Exposedport, err := net.SplitHostPort(string(byteValue))
	if err != nil {
		return nil, err
	}

	var response ResponseMAPPort
	response.IPAddress = host
	response.PortNo = Exposedport
	response.EntireAddress = string(byteValue)

	return &response, nil
}
