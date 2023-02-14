package server

import (
	"fmt"
	"github.com/Akilan1999/p2p-rendering-computation/server/docker"
	"net"
	"net/rpc"
)

const (
	port = "8089"
)

type Listener int

type Docker struct {
	docker *docker.DockerVM
}

// Starts container using RPC calls
func (l *Listener) StartContainer(reply *Docker) error {
	vm, err := docker.BuildRunContainer(3, "false", "")
	if err != nil {
		return err
	}
	fmt.Printf("Receive: %v\n", vm)
	*reply = Docker{vm}
	return nil
}

func Rpc() {
	rpcServer, err := net.ResolveTCPAddr("tcp", "0.0.0.0:"+port)
	if err != nil {
		fmt.Print(err)
	}
	inbound, err := net.ListenTCP("tcp", rpcServer)
	if err != nil {
		fmt.Print(err)
	}
	listener := new(Listener)
	rpc.Register(listener)
	rpc.Accept(inbound)
}
