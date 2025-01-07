package clientIPTable

import (
	"testing"
)

func TestUpdateIpTableListClient(t *testing.T) {
	err := UpdateIpTableListClient()

	if err != nil {
		t.Error(err)
	}
}
