package client

import (
	"fmt"
	"testing"
)

func TestUpdateIpTableListClient(t *testing.T) {
	err := UpdateIpTableListClient()

	if err != nil {
		t.Error(err)
	}
}

// Testing is a IPV6 address is returned
func TestGetCurrentIPV6(t *testing.T) {
	 res, err := GetCurrentIPV6()

	if err != nil {
		t.Error(err)
	}

	fmt.Println(res)
}