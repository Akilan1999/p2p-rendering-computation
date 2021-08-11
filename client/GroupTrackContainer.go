package client

import (
	"encoding/json"
	"git.sr.ht/~akilan1999/p2p-rendering-computation/config"
	"github.com/google/uuid"
	"io/ioutil"
	"os"
)

// Groups Data Structure type
type Groups struct {
	GroupList []*Group `json:"Groups"`
}

// Group Information about a single group
type Group struct {
	ID string `json:"ID"`
	TrackContainerList []*TrackContainer `json:"TrackContainer"`
}

// CreateGroup Creates a new group to add a set of trackcontainers
func CreateGroup() (*Group, error){
	// Creating variable of type new group
	var NewGroup Group
	// Generate new UUID for group ID
	id := uuid.New()
	// Add new group id and prepend with the string "grp"
	// The reason this is done is to differentiate between a
	// group ID and docker container ID
	NewGroup.ID = "grp" + id.String()
	// Adding the new group to the
	// GroupTrackContainer File
	err := NewGroup.AddGroupToFile()
	if err != nil {
		return nil, err
	}

	return &NewGroup,nil
}

// AddGroupToFile Adds Group struct to the GroupTrackContainer File
func (grp *Group) AddGroupToFile() error {
	// Gets all group information from the
	// GroupTrackContainer JSON file
    groups, err := ReadGroup()
    if err != nil {
    	return err
	}
	// Appending the newly created group
	groups.GroupList = append(groups.GroupList, grp)
	// Writing Group information to the GroupTrackContainer
	// JSON file
	err = groups.WriteGroup()
	if err != nil {
		return err
	}

	return nil
}

// ReadGroup Function reads grouptrackcontainers.json and converts
// result to Groups
func ReadGroup() (*Groups,error) {
	// Get Path from config
	config, err := config.ConfigInit()
	if err != nil {
		return nil,err
	}
	jsonFile, err := os.Open(config.GroupTrackContainersPath)
	// if we os.Open returns an error then handle it
	if err != nil {
		return nil,err
	}

	// defer the closing of our jsonFile so that we can parse it later on
	defer jsonFile.Close()

	// read our opened xmlFile as a byte array.
	byteValue, _ := ioutil.ReadAll(jsonFile)

	// we initialize our Users array
	var groups Groups

	// we unmarshal our byteArray which contains our
	// jsonFile's content into 'users' which we defined above
	json.Unmarshal(byteValue, &groups)
	return &groups, nil
}


// WriteGroup Function to write type Groups to the grouptrackcontainers.json file
func (grp *Groups)WriteGroup() error {
	file, err := json.MarshalIndent(grp, "", " ")
	if err != nil {
		return err
	}

	// Get Path from config
	config, err := config.ConfigInit()
	if err != nil {
		return err
	}
    // Writes to the appropriate file
	err = ioutil.WriteFile(config.GroupTrackContainersPath, file, 0644)
	if err != nil {
		return err
	}

	return nil
}