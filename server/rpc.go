package server

import (
	"fmt"
	"git.sr.ht/~akilan1999/p2p-rendering-computation/server/docker"
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
func (l *Listener) StartContainer( reply *Docker) error {
	fmt.Print("here")
	vm, err := docker.BuildRunContainer()
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

