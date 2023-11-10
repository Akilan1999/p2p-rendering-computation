package main

import "C"
import (
    "encoding/json"
    "github.com/Akilan1999/p2p-rendering-computation/abstractions"
    "github.com/Akilan1999/p2p-rendering-computation/client"
    "github.com/Akilan1999/p2p-rendering-computation/server"
)

// The Client package where data-types
// are manually converted to the
// to a string so that it can
// be export

//export StartContainer
func StartContainer(IP string, NumPorts int, GPU bool, ContainerName string, baseImage string) (output *C.char) {
    container, err := client.StartContainer(IP, NumPorts, GPU, ContainerName, baseImage)
    if err != nil {
        return C.CString(err.Error())
    }
    return ConvertStructToJSONString(container)
}

//export GetSpecs
func GetSpecs(IP string) (output *C.char) {
    specs, err := client.GetSpecs(IP)
    if err != nil {
        return C.CString(err.Error())
    }
    return ConvertStructToJSONString(specs)
}

//export Init
func Init(customConfig string) (output *C.char) {
    init, err := abstractions.Init(customConfig)
    if err != nil {
        return C.CString(err.Error())
    }
    return ConvertStructToJSONString(init)

}

//export Server
func Server() (output *C.char) {
    _, err := server.Server()
    if err != nil {
        return C.CString(err.Error())
    }

    return ConvertStructToJSONString("")
}

func ConvertStructToJSONString(Struct interface{}) *C.char {
    jsonBytes, err := json.Marshal(Struct)
    if err != nil {
        return C.CString(err.Error())
    }

    // Convert the JSON bytes to a string
    return C.CString(string(jsonBytes))
}

func main() {}
