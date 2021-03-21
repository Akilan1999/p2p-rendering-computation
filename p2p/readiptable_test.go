package p2p

import (
	"testing"
)

func TestReadIpTable(t *testing.T) {
	json, err := ReadIpTable()
	if err != nil {
		t.Fatal(err)
	}

	err = json.WriteIpTable()
	if err != nil {
		t.Fatal(err)
	}
}
