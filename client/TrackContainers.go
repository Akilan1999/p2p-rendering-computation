package client

import (
	"encoding/json"
	"errors"
	"fmt"
	"git.sr.ht/~akilan1999/p2p-rendering-computation/config"
	"git.sr.ht/~akilan1999/p2p-rendering-computation/server/docker"
	"io/ioutil"
	"os"
)

// TrackContainers This struct stores arrays of current containers running
type TrackContainers struct {
  	TrackContainerList []TrackContainer `json:"TrackContainer"`
}

// TrackContainer Stores information of current containers
type TrackContainer struct {
	Id        string            `json:"ID"`
	Container *docker.DockerVM  `json:"Container"`
	IpAddress string            `json:"IpAddress"`
}

// AddTrackContainer Adds new container which has been added to the track container
func AddTrackContainer(d *docker.DockerVM,ipAddress string) error {
	// Checking if pointer d is null
	if d == nil {
		return errors.New("d is nil")
	}
	//Get config information to derive paths for track containers json file
	config,err := config.ConfigInit()
	if err != nil {
		return err
	}

    // Getting information about the file trackcontainers.json file
    stat, err := os.Stat(config.TrackContainersPath)
	if err != nil {
		return err
	}
	// Initialize variable for TrackContainers
	var trackContainers TrackContainers
	// If the trackcontainers.json file is not empty then
	// Read from that file
	if stat.Size() != 0 {
		// Reads tracked container file
		trackContainersFile, err := ReadTrackContainers(config.TrackContainersPath)
		if err != nil {
			return err
		}
		trackContainers = *trackContainersFile
	}

	// Initialize new variable with type struct TrackContainers and
	// add container struct and ip address
	var trackContainer TrackContainer
	trackContainer.Id = d.ID
	trackContainer.Container = d
	trackContainer.IpAddress = ipAddress

	// Adds new container as passed in the parameter to the struct
	if &trackContainer == nil {
		return errors.New("trackContainer variable is nil")
	}
	trackContainers.TrackContainerList = append(trackContainers.TrackContainerList, trackContainer)

	// write modified information to the tracked json file
	data,err := json.MarshalIndent(trackContainers, "", "\t")
	if err != nil {
		return err
	}
	err = ioutil.WriteFile(config.TrackContainersPath,data,0777)
	if err != nil {
		return err
	}

	return nil
}

// RemoveTrackedContainer This function removos tracked container from the trackcontainer JSON file
func RemoveTrackedContainer(id string) error {
	//Get config information to derive paths for track containers json file
	config,err := config.ConfigInit()
	if err != nil {
		return err
	}
	// Getting tracked container struct
	trackedContainers, err := ReadTrackContainers(config.TrackContainersPath)
	// Storing index of element to remove
	var removeElement int
	removeElement = -1
	for i := range trackedContainers.TrackContainerList {
		if trackedContainers.TrackContainerList[i].Id == id {
			removeElement = i
			break
		}
	}
	// Checks if the element to be removed has been detected
	if removeElement == -1 {
		return errors.New("Container ID not found in the tracked list")
	}
	// Remove the detected element from the struct
	trackedContainers.TrackContainerList = append(trackedContainers.TrackContainerList[:removeElement], trackedContainers.TrackContainerList[removeElement+1:]...)

	// write modified information to the tracked json file
	data,err := json.MarshalIndent(trackedContainers, "", "\t")
	if err != nil {
		return err
	}
	err = ioutil.WriteFile(config.TrackContainersPath,data,0777)
	if err != nil {
		return err
	}

	return nil
}

// ViewTrackedContainers View Containers currently tracked
func ViewTrackedContainers() (error,*TrackContainers) {
	config,err := config.ConfigInit()
	if err != nil {
		return err,nil
	}
	trackedContianers, err := ReadTrackContainers(config.TrackContainersPath)
	if err != nil {
		return err,nil
	}

	return nil,trackedContianers
}

// ReadTrackContainers Reads containers which are currently tracked
func ReadTrackContainers(filename string) (*TrackContainers, error) {
	buf, err := ioutil.ReadFile(filename)
	if err != nil {
		return nil, err
	}

	c := &TrackContainers{}
	err = json.Unmarshal(buf, c)
	if err != nil {
		return nil, fmt.Errorf("in file %q: %v", filename, err)
	}

	return c, nil
}

// GetContainerInformation gets information about container based on
// container ID provided
func GetContainerInformation(ID string) (*TrackContainer, error) {
	// Getting the current containers
	err, CurrentContainers := ViewTrackedContainers()
	if err != nil {
		return nil, err
	}
	// Iterating through all tracked containers to get the container information
	// of the ID passed through the function parameter
	for _, container := range CurrentContainers.TrackContainerList {
		if container.Container.ID == ID {
			return &container, nil
		}
	}
	return nil, errors.New("Container not found. ")
}