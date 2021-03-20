package p2p

import (
	"encoding/json"
	"io/ioutil"
	"os"
)

// Get IP table Data

type IpAddresses struct {
	IpAddress []IpAddress `json:"ip_address"`
}

type IpAddress struct {
	Ipv4   string `json:"ipv4"`
	Latency  float32 `json:"latency"`
	Download float32    `json:"download"`
	Upload float32 `json:"upload"`
}

func ReadIpTable()(*IpAddresses ,error){

	jsonFile, err := os.Open("ip_table.json")
	// if we os.Open returns an error then handle it
	if err != nil {
		return nil,err
	}

	// defer the closing of our jsonFile so that we can parse it later on
	defer jsonFile.Close()

	// read our opened xmlFile as a byte array.
	byteValue, _ := ioutil.ReadAll(jsonFile)

	// we initialize our Users array
	var ipAddresses IpAddresses

	// we unmarshal our byteArray which contains our
	// jsonFile's content into 'users' which we defined above
	json.Unmarshal(byteValue, &ipAddresses)

    return &ipAddresses, nil
}
