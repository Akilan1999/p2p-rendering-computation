package client

import (
	"fmt"
	"git.sr.ht/~akilan1999/p2p-rendering-computation/server/docker"
	"testing"
)

// Tests a scenario where the container are getting tracked
func TestAddTrackContainer(t *testing.T) {
	// Create docker container and get SSH port
	container1 ,err := docker.BuildRunContainer(0,"false","")
	if err != nil {
		fmt.Println(err)
		t.Fail()
	}
	// Testing the AddTrackContainer Function
	err = AddTrackContainer(container1,"0.0.0.0")
	if err != nil {

		err = docker.StopAndRemoveContainer(container1.ID)
		if err != nil {
			fmt.Println(err)
			t.Fail()
		}
		fmt.Println(err)
		t.Fail()
	}

	err = docker.StopAndRemoveContainer(container1.ID)
	if err != nil {
		fmt.Println(err)
		t.Fail()
	}
}
