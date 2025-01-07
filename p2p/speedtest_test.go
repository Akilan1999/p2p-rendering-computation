package p2p

import (
	"testing"
)

// To run this test ip_table.json must be populated
func TestServer_SpeedTest(t *testing.T) {
	err := LocalSpeedTestIpTable()
	if err != nil {
		t.Fatal(err)
	}

	//HumaidTest("http://localhost:8088/50")
	//HumaidTest("http://ipv4.download.thinkbroadband.com/50MB.zip")
}
