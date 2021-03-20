package p2p

import (
	"testing"
)

func TestReadIpTable(t *testing.T) {
	_, err := ReadIpTable()
	if err != nil {
		t.Fatal(err)
	}
}
