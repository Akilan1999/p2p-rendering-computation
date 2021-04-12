package client

import (
	"bytes"
	"encoding/json"
	"git.sr.ht/~akilan1999/p2p-rendering-computation/p2p"
	"io"
	"io/ioutil"
	"mime/multipart"
	"net/http"
	"os"
	"path/filepath"
)

// Does the following to update it's IP table
func UpdateIpTable(IpAddress string) error {

	resp, err := SendPostRequest("http://"+IpAddress+":8088/IpTable",
		"/etc/p2p-rendering/ip_table.json",
		"json")

	if err != nil {
		return err
	}

	var ipStruct p2p.IpAddresses
	json.Unmarshal(resp, &ipStruct)

	// Updates IP table based on information provided
	// by the server
	err = ipStruct.SpeedTestUpdatedIPTable()
	if err != nil {
		return err
	}

	return nil
}

// UpdateIpTableListClient updates IP tables (Default 3 hops) based on server information available
//on the ip tables
func UpdateIpTableListClient() error {
	// Ensure that the IP Table has Node pingable
	err := p2p.LocalSpeedTestIpTable()
	if err != nil {
		return err
	}

	// IP addresses to not append to struct due to
	// duplication

	Addresses, err := p2p.ReadIpTable()
	DoNotRead := Addresses

	// Run loop 3 times
	for i := 0; i < 3; i++ {
		// Gets information from IP table
		Addresses, err = p2p.ReadIpTable()
		if err != nil {
			return err
		}

		// Updates IP table based on server IP table
		for j := range Addresses.IpAddress {

			// Check if IP addresses is there in the struct DoNotRead
			Exists := false
			for k := range DoNotRead.IpAddress {
				if DoNotRead.IpAddress[k].Ipv4 == Addresses.IpAddress[j].Ipv4 {
					Exists = true
					break
				}
			}

			// If the struct exists then continues
			if Exists {
				continue
			}

			err = UpdateIpTable(Addresses.IpAddress[j].Ipv4)
			if err != nil {
				return err
			}

			//Appends server1 IP address to variable DoNotRead
			DoNotRead.IpAddress = append(DoNotRead.IpAddress, Addresses.IpAddress[j])
		}
	}

	return nil
}

// SendPostRequest Sends a file as a POST request.
// Reference (https://stackoverflow.com/questions/51234464/upload-a-file-with-post-request-golang)
func SendPostRequest (url string, filename string, filetype string) ([]byte,error) {
	file, err := os.Open(filename)
	if err != nil {
		return nil,err
	}
	defer file.Close()


	body := &bytes.Buffer{}
	writer := multipart.NewWriter(body)
	part, err := writer.CreateFormFile(filetype, filepath.Base(file.Name()))

	if err != nil {
		return nil,err
	}

	io.Copy(part, file)
	writer.Close()
	request, err := http.NewRequest("POST", url, body)

	if err != nil {
		return nil,err
	}

	request.Header.Add("Content-Type", writer.FormDataContentType())
	client := &http.Client{}

	response, err := client.Do(request)

	if err != nil {
		return nil,err
	}
	defer response.Body.Close()

	content, err := ioutil.ReadAll(response.Body)

	if err != nil {
		return nil,err
	}

	return content, nil
}