package client

import (
	"fmt"
	"github.com/Akilan1999/p2p-rendering-computation/server/docker"
	"testing"
)

// Tests a scenario where the container are getting tracked
func TestAddTrackContainer(t *testing.T) {
	// Create docker container and get SSH port
	container1, err := docker.BuildRunContainer(0, "false", "")
	if err != nil {
		fmt.Println(err)
		t.Fail()
	}
	// Testing the AddTrackContainer Function
	err = AddTrackContainer(container1, "0.0.0.0")
	if err != nil {
		// Killing docker container created
		err = docker.StopAndRemoveContainer(container1.ID)
		if err != nil {
			fmt.Println(err)
			t.Fail()
		}
		fmt.Println(err)
		t.Fail()
	}
	// Killing docker container created
	err = docker.StopAndRemoveContainer(container1.ID)
	if err != nil {
		fmt.Println(err)
		t.Fail()
	}
}

// Testing the remove container function
// NOTE: This test can also be considered as a whole flow on the process of
// tracked containers
func TestRemoveTrackedContainer(t *testing.T) {
	container1, err := docker.BuildRunContainer(0, "false", "")
	if err != nil {
		fmt.Println(err)
		t.Fail()
	}

	container2, err := docker.BuildRunContainer(0, "false", "")
	if err != nil {
		fmt.Println(err)
		t.Fail()
	}

	// Testing the AddTrackContainer Function and adding the first container created
	err = AddTrackContainer(container1, "0.0.0.0")
	if err != nil {
		// Killing docker container created
		err = docker.StopAndRemoveContainer(container1.ID)
		if err != nil {
			fmt.Println(err)
			t.Fail()
		}
		fmt.Println(err)
		t.Fail()
	}
	// Killing docker container created
	err = docker.StopAndRemoveContainer(container1.ID)
	if err != nil {
		fmt.Println(err)
		t.Fail()
	}

	// Testing the AddTrackContainer Function and the adding the second container created
	err = AddTrackContainer(container2, "0.0.0.0")
	if err != nil {
		// Killing docker container created
		err = docker.StopAndRemoveContainer(container2.ID)
		if err != nil {
			fmt.Println(err)
			t.Fail()
		}
		fmt.Println(err)
		t.Fail()
	}
	// Killing docker container created
	err = docker.StopAndRemoveContainer(container2.ID)
	if err != nil {
		fmt.Println(err)
		t.Fail()
	}

	// Removing container 1 from the tracked list
	err = RemoveTrackedContainer(container1.ID)
	if err != nil {
		fmt.Println(err)
		t.Fail()
	}

	// Removing container 2 from the tracked list
	err = RemoveTrackedContainer(container2.ID)
	if err != nil {
		fmt.Println(err)
		t.Fail()
	}
}

// Test function that checks if the ID belongs to
// a group or container running
func TestCheckID(t *testing.T) {
	id := "grp123"
	checkID, err := CheckID(id)
	if err != nil {
		fmt.Println(err)
		t.Fail()
	}
	if checkID == "group" {
		fmt.Println("pass")
	} else {
		t.Fail()
	}
}
