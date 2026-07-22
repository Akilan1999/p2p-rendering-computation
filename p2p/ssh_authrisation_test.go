package p2p

import (
	"fmt"
	"testing"
)

func TestReadAuthorizedKeys(t *testing.T) {
	err := RemoveKeyFromAuthorizedKeys("ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAAAgQC6UIyGxXae7fZXJ8p+43UtrA+d/jsnuxS6waOLClaqGczzXYvB21UjwxhdAH13GTvVAGMmdGNDpUSfaHBw2i/eLrG4IlorzNSFMyllpGVaL9mu7uM9V0pY5kWp0434NGUBZO/bCUOQRQzdIXJgo73vA7k1D8d8QT9XGmW8GYt6gQ==")
	if err != nil {
		fmt.Println(err)
	}
	fmt.Println("success")
}
