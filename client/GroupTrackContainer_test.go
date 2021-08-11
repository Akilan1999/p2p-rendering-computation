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

func TestRemoveGroup(t *testing.T) {
	// Creates a new group
	group, err := CreateGroup()
	if err != nil {
		fmt.Println(err)
		t.Fail()
	}
	// Removes the new group
	// creates
	err = RemoveGroup(group.ID)
	if err != nil {
		fmt.Println(err)
		t.Fail()
	}
}
