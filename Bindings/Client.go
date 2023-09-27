package Bindings

import "C"
import (
	"encoding/json"
	"github.com/Akilan1999/p2p-rendering-computation/client"
)

// The Client package where data-types
// are manually converted to the
// to a string so that it can
// be export

//export StartContainer
func StartContainer(IP string, NumPorts int, GPU bool, ContainerName string, baseImage string) (output string) {
	container, err := client.StartContainer(IP, NumPorts, GPU, ContainerName, baseImage)
	if err != nil {
		return err.Error()
	}

	jsonBytes, err := json.Marshal(container)
	if err != nil {
		return err.Error()
	}

	// Convert the JSON bytes to a string
	return string(jsonBytes)
}

//export GetSpecs
func GetSpecs(IP string) (output string) {
	specs, err := client.GetSpecs(IP)
	if err != nil {
		return err.Error()
	}

	jsonBytes, err := json.Marshal(specs)
	if err != nil {
		return err.Error()
	}

	// Convert the JSON bytes to a string
	return string(jsonBytes)
}
