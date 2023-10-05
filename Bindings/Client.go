package main

import "C"
import (
    "encoding/json"
    "github.com/Akilan1999/p2p-rendering-computation/abstractions"
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
    return ConvertStructToJSONString(container)
}

//export GetSpecs
func GetSpecs(IP string) (output string) {
    specs, err := client.GetSpecs(IP)
    if err != nil {
        return err.Error()
    }
    return ConvertStructToJSONString(specs)
}

//export Init
func Init(customConfig string) (output string) {
    init, err := abstractions.Init(customConfig)
    if err != nil {
        return err.Error()
    }
    return ConvertStructToJSONString(init)

}

func ConvertStructToJSONString(Struct interface{}) string {
    jsonBytes, err := json.Marshal(Struct)
    if err != nil {
        return err.Error()
    }

    // Convert the JSON bytes to a string
    return string(jsonBytes)
}

func main() {}
