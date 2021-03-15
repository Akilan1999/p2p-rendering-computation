package p2p

import (
  "fmt"
	"gitlab.com/NebulousLabs/go-upnp"
)

// Port forwarding to the router
func ForwardPort(port int) error{
    // connect to router
    d, err := upnp.Discover()
    if err != nil {
        return err
    }

    // discover external IP
    ip, err := d.ExternalIP()
    if err != nil {
        return err
    }
    fmt.Println("Your external IP is:", ip)

    // forward a port
    err = d.Forward(50498, "upnp test")
    if err != nil {
        return err
    }

    // record router's location
    loc := d.Location()

    // connect to router directly
    d, err = upnp.Load(loc)
    if err != nil {
        return err
    }

    return nil
}

// unForwardPort from router
func UnForwardPort(port int) error{
  // connect to router
  d, err := upnp.Discover()
  if err != nil {
      return err
  }


  // discover external IP
  ip, err := d.ExternalIP()
  if err != nil {
      return err
  }

  fmt.Println("Your external IP is:", ip)


  // un-forward a port
  err = d.Clear(50498)
  if err != nil {
      return err
  }

  // record router's location
  loc := d.Location()

  // connect to router directly
  d, err = upnp.Load(loc)
  if err != nil {
      return err
  }

  return nil

}
