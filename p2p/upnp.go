package main

import (
  "fmt"
	"log"
	"gitlab.com/NebulousLabs/go-upnp"
)

func main() {
    // connect to router
    d, err := upnp.Discover()
    if err != nil {
        log.Fatal(err)
    }

    // discover external IP
    ip, err := d.ExternalIP()
    if err != nil {
        log.Fatal(err)
    }
    fmt.Println("Your external IP is:", ip)

    // forward a port
  /*   err = d.Forward(63390, "upnp test")
    if err != nil {
        log.Fatal(err)
    } */

    // un-forward a port
    err = d.Clear(63390)
    if err != nil {
        log.Fatal(err)
    }

    // record router's location
    loc := d.Location()

    // connect to router directly
    d, err = upnp.Load(loc)
    if err != nil {
        log.Fatal(err)
    }
}
