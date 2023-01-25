package p2p

import (
    "fmt"
    "testing"
)

// Tests if the current has UPNP support
func TestForwardUPNPPort(t *testing.T) {
    err := ForwardPort(6586)
    if err != nil {
        fmt.Println(err)
        t.Fail()
    }
}
