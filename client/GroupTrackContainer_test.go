package client

import (
	"fmt"
	"testing"
)

// Testing out if a new group is getting created
func TestCreateGroup(t *testing.T) {
	group, err := CreateGroup()
	if err != nil {
		fmt.Println(err)
		t.Fail()
	}
	PrettyPrint(group)
}
