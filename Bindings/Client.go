package main

import "C"
import (
	"encoding/json"
	"github.com/Akilan1999/p2p-rendering-computation/abstractions"
	"github.com/Akilan1999/p2p-rendering-computation/client"
	"github.com/Akilan1999/p2p-rendering-computation/client/clientIPTable"
	"github.com/Akilan1999/p2p-rendering-computation/p2p"
	"github.com/Akilan1999/p2p-rendering-computation/plugin"
	"github.com/Akilan1999/p2p-rendering-computation/server"
)

// The Client package where data-types
// are manually converted to the
// to a string so that it can
// be export

// --------------------------------- Container Control ----------------------------------------

//export StartContainer
func StartContainer(IP string, NumPorts int, GPU bool, ContainerName string, baseImage string) (output *C.char) {
	container, err := client.StartContainer(IP, NumPorts, GPU, ContainerName, baseImage)
	if err != nil {
		return C.CString(err.Error())
	}
	return ConvertStructToJSONString(container)
}

//export RemoveContainer
func RemoveContainer(IP string, ID string) (output *C.char) {
	err := client.RemoveContianer(IP, ID)
	if err != nil {
		return C.CString(err.Error())
	}
	return C.CString("Success")
}

// --------------------------------- Plugin Control ----------------------------------------

//export ViewPlugin
func ViewPlugin() (output *C.char) {
	plugins, err := plugin.DetectPlugins()
	if err != nil {
		return C.CString(err.Error())
	}
	return ConvertStructToJSONString(plugins)
}

//export PullPlugin
func PullPlugin(pluginUrl string) (output *C.char) {
	err := plugin.DownloadPlugin(pluginUrl)
	if err != nil {
		return C.CString(err.Error())
	}
	return C.CString("Success")
}

//export DeletePlugin
func DeletePlugin(pluginName string) (output *C.char) {
	err := plugin.DeletePlugin(pluginName)
	if err != nil {
		return C.CString(err.Error())
	}
	return C.CString("Success")
}

//export ExecutePlugin
func ExecutePlugin(pluginname string, ContainerID string) (output *C.char) {
	err := plugin.RunPluginContainer(pluginname, ContainerID)
	if err != nil {
		return C.CString(err.Error())
	}
	return C.CString("Success")
}

// --------------------------------- Get Specs ----------------------------------------

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

// --------------------------------- P2P Controls -----------------------------------

//export ViewIPTable
func ViewIPTable() (output *C.char) {
	table, err := p2p.ReadIpTable()
	if err != nil {
		return C.CString(err.Error())
	}
	return ConvertStructToJSONString(table)
}

//export UpdateIPTable
func UpdateIPTable() (output *C.char) {
	err := clientIPTable.UpdateIpTableListClient()
	if err != nil {
		return C.CString(err.Error())
	}
	return C.CString("Success")
}

// --------------------------------- Controlling Server  ----------------------------------------

//export Server
func Server() (output *C.char) {
	_, err := server.Server()
	if err != nil {
		return C.CString(err.Error())
	}

	return ConvertStructToJSONString("")
}

// --------------------------------- Helper Functions  ----------------------------------------

func ConvertStructToJSONString(Struct interface{}) *C.char {
	jsonBytes, err := json.Marshal(Struct)
	if err != nil {
		return C.CString(err.Error())
	}

	// Convert the JSON bytes to a string
	return C.CString(string(jsonBytes))
}

func main() {}
