package frp

import (
	"fmt"
	"testing"
	"time"
)

// Testing scenario FRPServer
func TestStartFRPServer(t *testing.T) {
	var s Server
	s.IPAddress = "127.0.0.1"
	s.Port = 8808
	err := s.StartFRPServer(false)
	if err != nil {
		fmt.Println(err)
		t.Fail()
	}
}

// Testing scenario FRPServer and FRPClient connection
func TestStartFRPClient(t *testing.T) {
	var s Server
	s.IPAddress = "127.0.0.1"
	s.Port = 8808
	go s.StartFRPServer(false)

	time.Sleep(3 * time.Second)

	// Sample test client
	var c Client
	c.Server = &s
	c.ClientMappings = []ClientMapping{
		{
			LocalIP:    "127.0.0.1",
			LocalPort:  22,
			RemotePort: 3301,
		},
	}

	err := c.StartFRPClient()
	if err != nil {
		fmt.Println(err)
		t.Fail()
	}
}
