package client

import (
    "encoding/json"
    "errors"
    "github.com/Akilan1999/p2p-rendering-computation/config"
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
    ID                 string            `json:"ID"`
    TrackContainerList []*TrackContainer `json:"TrackContainer"`
    // Sneaky as required only when removing the element
    // Set when GetGroup function is called
    index int
}

// CreateGroup Creates a new group to add a set of track containers
func CreateGroup() (*Group, error) {
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

    return &NewGroup, nil
}

// RemoveGroup Removes group based on the group ID provided
func RemoveGroup(GroupID string) error {
    // Read group information from the
    //grouptrackcontainer json file
    groups, err := ReadGroup()
    if err != nil {
        return err
    }
    // Gets Group struct based on group ID
    // provided
    group, err := GetGroup(GroupID)
    if err != nil {
        return err
    }
    // Remove Group struct from the groups variable
    groups.GroupList = append(groups.GroupList[:group.index], groups.GroupList[group.index+1:]...)

    // Write new groups to the grouptrackcontainer json file
    err = groups.WriteGroup()
    if err != nil {
        return err
    }

    return nil
}

// AddContainerToGroup Adds container information to the Group based on the Group ID
func AddContainerToGroup(ContainerID string, GroupID string) (*Group, error) {
    // Gets container information based on container ID provided
    containerInfo, err := GetContainerInformation(ContainerID)
    if err != nil {
        return nil, err
    }
    // Gets group information based on the group ID provided
    group, err := GetGroup(GroupID)
    if err != nil {
        return nil, err
    }
    // Adds container information the group
    group.AddContainer(containerInfo)

    // Get Groups information from reading the grouptrackcontainer.json file
    groups, err := ReadGroup()
    if err != nil {
        return nil, err
    }
    // Updating specific element in the group list with the added container
    groups.GroupList[group.index] = group
    // Write groups information on the grouptrackcontainer.json file
    err = groups.WriteGroup()
    if err != nil {
        return nil, err
    }

    return group, nil
}

// RemoveContainerGroup Remove Container from the group ID specified
func RemoveContainerGroup(ContainerID string, GroupID string) (*Group, error) {
    // Get container information based on container ID provided
    containerInfo, err := GetContainerInformation(ContainerID)
    if err != nil {
        return nil, err
    }
    // Gets group information based on the group ID provided
    group, err := GetGroup(GroupID)
    if err != nil {
        return nil, err
    }
    // Remove container from the appropriate group
    err = group.RemoveContainerGroup(containerInfo)
    if err != nil {
        return nil, err
    }
    // Get Groups information from reading the grouptrackcontainer.json file
    groups, err := ReadGroup()
    if err != nil {
        return nil, err
    }
    // Updating specific element in the group list with the remove container
    groups.GroupList[group.index] = group
    // Write groups information on the grouptrackcontainer.json file
    err = groups.WriteGroup()
    if err != nil {
        return nil, err
    }

    return group, nil
}

// RemoveContainerGroups Remove Container from groups (i.e which ever groups has information
// about that container). This is mostly called when a container is deleted or removed from
// the tracked container list
func RemoveContainerGroups(ContainerID string) error {
    // Get container information based on container ID provided
    containerInfo, err := GetContainerInformation(ContainerID)
    if err != nil {
        return err
    }
    // Get Groups information from reading the grouptrackcontainer.json file
    groups, err := ReadGroup()
    if err != nil {
        return err
    }
    // Removes container information from all groups it is found in
    err = groups.RemoveContainerGroups(containerInfo)
    if err != nil {
        return err
    }
    // Write groups information on the grouptrackcontainer.json file
    err = groups.WriteGroup()
    if err != nil {
        return err
    }

    return nil
}

// GetGroup Gets group information based on
// group id provided
func GetGroup(GroupID string) (*Group, error) {
    // Read group information from the
    //grouptrackcontainer json file
    groups, err := ReadGroup()
    if err != nil {
        return nil, err
    }
    // Iterate through the set of groups and
    // if the group ID matches then return it
    for i, group := range groups.GroupList {
        if group.ID == GroupID {
            group.index = i
            return group, nil
        }
    }

    return nil, errors.New("Group not found. ")
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
func ReadGroup() (*Groups, error) {
    // Get Path from config
    config, err := config.ConfigInit(nil, nil)
    if err != nil {
        return nil, err
    }
    jsonFile, err := os.Open(config.GroupTrackContainersPath)
    // if we os.Open returns an error then handle it
    if err != nil {
        return nil, err
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
func (grp *Groups) WriteGroup() error {
    file, err := json.MarshalIndent(grp, "", " ")
    if err != nil {
        return err
    }
    // Get Path from config
    config, err := config.ConfigInit(nil, nil)
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

// AddContainer Adds a container to the Tracked container list of the group
func (grp *Group) AddContainer(Container *TrackContainer) error {
    grp.TrackContainerList = append(grp.TrackContainerList, Container)
    return nil
}

// RemoveContainerGroup Removes container information from the group
func (grp *Group) RemoveContainerGroup(Container *TrackContainer) error {
    // Iterating through all container in the Group of Tracked Container
    for i, container := range grp.TrackContainerList {
        // If the container ID matches then remove the container from the group
        if container.Id == Container.Id {
            grp.TrackContainerList = append(grp.TrackContainerList[:i], grp.TrackContainerList[i+1:]...)
        }
    }
    return nil
}

// RemoveContainerGroups removes container found in all groups
func (grp *Groups) RemoveContainerGroups(Container *TrackContainer) error {
    // Iterating through all groups
    for i, group := range grp.GroupList {
        // Removes the container in the following group
        // if it exists
        err := group.RemoveContainerGroup(Container)
        if err != nil {
            return err
        }
        // Set group information to list groups
        grp.GroupList[i] = group
    }
    return nil
}

// ModifyContainerGroups Modifies container information is all groups
// available
func (TC *TrackContainer) ModifyContainerGroups() error {
    group, err := ReadGroup()
    if err != nil {
        return err
    }
    // Iterate though all groups and modify the container
    // information in groups where the modified container
    // ID matches
    for i, _ := range group.GroupList {
        // Checking in each group if the modified container ID exists
        for j, _ := range group.GroupList[i].TrackContainerList {
            // If there is match then change them
            if group.GroupList[i].TrackContainerList[j].Id == TC.Id {
                group.GroupList[i].TrackContainerList[j] = TC
            }
        }
    }

    // Write modified result to the Groups track container JSON file
    err = group.WriteGroup()
    if err != nil {
        return err
    }

    return nil
}
