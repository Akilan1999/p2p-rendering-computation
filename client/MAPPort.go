package client

import (
	"github.com/Akilan1999/p2p-rendering-computation/config"
	"io/ioutil"
	"net/http"
)

func MAPPort(port string, domainName string) (string, error) {
	Config, err := config.ConfigInit(nil, nil)
	if err != nil {
		return "", err
	}

	//if version == "version 6" {
	URL := "http://0.0.0.0:" + Config.ServerPort + "/MAPPort?port=" + port + "&domain_name=" + domainName
	//} else {
	//	URL = "http://" + IP + ":" + serverPort + "/server_info"
	//}
	resp, err := http.Get(URL)
	if err != nil {
		return "", err
	}

	// Convert response to byte value
	byteValue, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		return "", err
	}

	return string(byteValue), nil
}
