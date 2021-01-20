package main

import (
	"net"

	"github.com/pion/mdns"
	"golang.org/x/net/ipv4"
)

func main() {
	server, err := zeroconf.Register("GoZeroconf", "_workstation._tcp", "local.", 42424, []string{"txtv=0", "lo=1", "la=2"}, nil)
    if err != nil {
       panic(err)
    }
    defer server.Shutdown()

    // Clean exit.
    sig := make(chan os.Signal, 1)
    signal.Notify(sig, os.Interrupt, syscall.SIGTERM)
    select {
       case <-sig:
       // Exit by user
       case <-time.After(time.Second * 120):
       // Exit by timeout
    }

    log.Println("Shutting down.")
}