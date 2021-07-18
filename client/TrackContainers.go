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
  	TrackcontianerList []TrackContainer `json:"TrackContainer"`
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
	trackContainers.TrackcontianerList = append(trackContainers.TrackcontianerList, trackContainer)

	// write modified information to the tracked json file
	data,err := json.Marshal(trackContainers)
	if err != nil {
		return err
	}
	err = ioutil.WriteFile(config.TrackContainersPath,data,0777)
	if err != nil {
		return err
	}

	return nil
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