package p2p

import(
  "testing"
)

func TestAddRemoveUpnp(t *testing.T){

   // forwarding port 23241 via upnp
   err := ForwardPort(23241)
   if  err != nil {
     t.Errorf("Error returned: %q", err)
   }

   // unforwarding port 23241 via upnp
   err = UnForwardPort(23241)
   if err != nil {
     t.Errorf("Error returned: %q", err)
   }

}
