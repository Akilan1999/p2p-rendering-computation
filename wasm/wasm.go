package main

import (
	"encoding/json"
	"errors"
	"time"

	"syscall/js"

	"github.com/Akilan1999/p2p-rendering-computation/abstractions"
	"github.com/Akilan1999/p2p-rendering-computation/p2p/frp"
)

func main() {
	// Register functions
	js.Global().Set("getSpecs", js.FuncOf(getSpecs))
	js.Global().Set("initConfig", js.FuncOf(initConfig))
	js.Global().Set("viewIPTable", js.FuncOf(viewIPTable))
	js.Global().Set("updateIPTable", js.FuncOf(updateIPTable))
	js.Global().Set("escapeFirewall", js.FuncOf(escapeFirewall))
	js.Global().Set("mapPort", js.FuncOf(mapPort))
	js.Global().Set("customInformation", js.FuncOf(customInformation))
	js.Global().Set("addRootNode", js.FuncOf(addRootNode))
	js.Global().Set("startServer", js.FuncOf(startServer))

	// Prevent Go program from exiting
	select {}
}

// ------------------------------------- Helpers ----------------------------------------

func convertToJSValue(value interface{}) js.Value {
	jsonBytes, err := json.Marshal(value)
	if err != nil {
		return js.ValueOf(map[string]interface{}{"error": err.Error()})
	}
	return js.ValueOf(string(jsonBytes))
}

func returnError(err error) js.Value {
	return js.ValueOf(map[string]interface{}{"error": err.Error()})
}

// ------------------------------------- WASM Functions ----------------------------------------

func getSpecs(this js.Value, args []js.Value) any {
	if len(args) < 1 {
		return returnError(jsError("Missing IP argument"))
	}
	specs, err := abstractions.GetSpecs(args[0].String())
	if err != nil {
		return returnError(err)
	}
	return convertToJSValue(specs)
}

func initConfig(this js.Value, args []js.Value) any {
	if len(args) < 1 {
		return returnError(jsError("Missing config argument"))
	}
	initData, err := abstractions.Init(args[0].String())
	if err != nil {
		return returnError(err)
	}
	return convertToJSValue(initData)
}

func viewIPTable(this js.Value, args []js.Value) any {
	table, err := abstractions.ViewIPTable()
	if err != nil {
		return returnError(err)
	}
	return convertToJSValue(table)
}

func updateIPTable(this js.Value, args []js.Value) any {
	err := abstractions.UpdateIPTable()
	if err != nil {
		return returnError(err)
	}
	return js.ValueOf("Success")
}

func escapeFirewall(this js.Value, args []js.Value) any {
	if len(args) < 3 {
		return returnError(jsError("Expected 3 arguments"))
	}
	host := args[0].String()
	port := args[1].String()
	internalPort := args[2].String()

	serverPort, err := frp.GetFRPServerPort("http://" + host + ":" + port)
	if err != nil {
		return returnError(err)
	}

	time.Sleep(5 * time.Second)

	exposedPort, err := frp.StartFRPClientForServer(host+":"+port, serverPort, internalPort, "")
	if err != nil {
		return returnError(err)
	}

	return js.ValueOf(exposedPort)
}

func mapPort(this js.Value, args []js.Value) any {
	if len(args) < 3 {
		return returnError(jsError("Expected 3 arguments"))
	}
	port := args[0].String()
	domain := args[1].String()
	server := args[2].String()

	addr, err := abstractions.MapPort(port, domain, server)
	if err != nil {
		return returnError(err)
	}
	return js.ValueOf(addr.EntireAddress)
}

func customInformation(this js.Value, args []js.Value) any {
	if len(args) < 1 {
		return returnError(jsError("Missing custom information"))
	}
	err := abstractions.AddCustomInformation(args[0].String())
	if err != nil {
		return returnError(err)
	}
	return js.ValueOf("Success")
}

func addRootNode(this js.Value, args []js.Value) any {
	if len(args) < 2 {
		return returnError(jsError("Expected 2 arguments"))
	}
	err := abstractions.AddRootNode(args[0].String(), args[1].String())
	if err != nil {
		return returnError(err)
	}
	return js.ValueOf("Success")
}

func startServer(this js.Value, args []js.Value) any {
	_, err := abstractions.Start()
	if err != nil {
		return returnError(err)
	}
	return js.ValueOf("Server started")
}

// ------------------------------------- Utility ----------------------------------------

func jsError(msg string) error {
	return errors.New("WASM error: " + msg)
}
