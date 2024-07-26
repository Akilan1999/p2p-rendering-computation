package main

import "C"
import (
	"encoding/json"
	"time"

	"github.com/Akilan1999/p2p-rendering-computation/abstractions"
	"github.com/Akilan1999/p2p-rendering-computation/p2p/frp"
)

// The Client package where data-types
// are manually converted to the
// to a string so that it can
// be export

// --------------------------------- Container Control ----------------------------------------

//export StartContainer
func StartContainer(IP *C.char) (output *C.char) {
	container, err := abstractions.StartContainer(C.GoString(IP))
	if err != nil {
		return C.CString(err.Error())
	}
	return ConvertStructToJSONString(container)
}

//export RemoveContainer
func RemoveContainer(IP *C.char, ID *C.char) (output *C.char) {
	err := abstractions.RemoveContainer(C.GoString(IP), C.GoString(ID))
	if err != nil {
		return C.CString(err.Error())
	}
	return C.CString("Success")
}

// --------------------------------- Plugin Control ----------------------------------------

// DEPRECATED
////export ViewPlugin
//func ViewPlugin() (output *C.char) {
//	plugins, err := plugin.DetectPlugins()
//	if err != nil {
//		return C.CString(err.Error())
//	}
//	return ConvertStructToJSONString(plugins)
//}
//
////export PullPlugin
//func PullPlugin(pluginUrl string) (output *C.char) {
//	err := plugin.DownloadPlugin(pluginUrl)
//	if err != nil {
//		return C.CString(err.Error())
//	}
//	return C.CString("Success")
//}
//
////export DeletePlugin
//func DeletePlugin(pluginName string) (output *C.char) {
//	err := plugin.DeletePlugin(pluginName)
//	if err != nil {
//		return C.CString(err.Error())
//	}
//	return C.CString("Success")
//}
//
////export ExecutePlugin
//func ExecutePlugin(pluginname string, ContainerID string) (output *C.char) {
//	err := plugin.RunPluginContainer(pluginname, ContainerID)
//	if err != nil {
//		return C.CString(err.Error())
//	}
//	return C.CString("Success")
//}

// --------------------------------- Get Specs ----------------------------------------

//export GetSpecs
func GetSpecs(IP *C.char) (output *C.char) {
	specs, err := abstractions.GetSpecs(C.GoString(IP))
	if err != nil {
		return C.CString(err.Error())
	}
	return ConvertStructToJSONString(specs)
}

//export Init
func Init(customConfig *C.char) (output *C.char) {
	init, err := abstractions.Init(C.GoString(customConfig))
	if err != nil {
		return C.CString(err.Error())
	}
	return ConvertStructToJSONString(init)
}

// --------------------------------- P2P Controls -----------------------------------

//export ViewIPTable
func ViewIPTable() (output *C.char) {
	table, err := abstractions.ViewIPTable()
	if err != nil {
		return C.CString(err.Error())
	}
	return ConvertStructToJSONString(table)
}

//export UpdateIPTable
func UpdateIPTable() (output *C.char) {
	err := abstractions.UpdateIPTable()
	if err != nil {
		return C.CString(err.Error())
	}
	return C.CString("Success")
}

//export EscapeFirewall
func EscapeFirewall(HostOutsideNATIP *C.char, HostOutsideNATPort *C.char, internalPort *C.char) (output *C.char) {
	// Get free port from P2PRC server node
	serverPort, err := frp.GetFRPServerPort("http://" + C.GoString(HostOutsideNATIP) + ":" + C.GoString(HostOutsideNATPort))

	if err != nil {
		return C.CString(err.Error())
	}

	time.Sleep(5 * time.Second)

	ExposedPort, err := frp.StartFRPClientForServer(C.GoString(HostOutsideNATIP)+":"+C.GoString(HostOutsideNATPort), C.GoString(serverPort), C.GoString(internalPort), "")
	if err != nil {
		return C.CString(err.Error())
	}

	return C.CString(ExposedPort)
}

//export MapPort
func MapPort(Port *C.char) *C.char {
	entireAddress, _, err := abstractions.MapPort(C.GoString(Port))
	if err != nil {
		return C.CString(err.Error())
	}
	return C.CString(entireAddress)
}

// --------------------------------- Controlling Server  ----------------------------------------

//export Server
func Server() (output *C.char) {
	_, err := abstractions.Start()
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
