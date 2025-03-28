package clientIPTable

import (
	"encoding/json"
	"fmt"
	"io"
	"io/ioutil"
	"mime/multipart"
	"net/http"
	"os"
	"sync"

	"github.com/Akilan1999/p2p-rendering-computation/config"
	"github.com/Akilan1999/p2p-rendering-computation/p2p"
)

var mu sync.Mutex

// UpdateIpTable Does the following to update it's IP table
func UpdateIpTable(IpAddress string, serverPort string, wg *sync.WaitGroup) error {

	config, err := config.ConfigInit(nil, nil)
	if err != nil {
		return err
	}

	client := http.Client{}

	var resp []byte

	version := p2p.Ip4or6(IpAddress)
	if version == "version 6" {
		resp, err = UploadMultipartFile(client, "http://["+IpAddress+"]:"+serverPort+"/IpTable", "json", config.IPTable)
		if err != nil {
			return err
		}
	} else {
		resp, err = UploadMultipartFile(client, "http://"+IpAddress+":"+serverPort+"/IpTable", "json", config.IPTable)
		if err != nil {
			return err
		}
	}

	if resp == nil {
		return nil
	}

	//resp, err := SendPostRequest("http://"+IpAddress+":8088/IpTable",
	//	config.IPTable,
	//	"json")
	//
	//if err != nil {
	//	return err
	//}

	var ipStruct p2p.IpAddresses
	json.Unmarshal(resp, &ipStruct)

	// Updates IP table based on information provided
	// by the server
	if len(ipStruct.IpAddress) > 0 {
		err = ipStruct.SpeedTestUpdatedIPTable()
		if err != nil {
			return err
		}
	}

	// Not required to update IP table as speed test updates the IP Table
	//err = ipStruct.WriteIpTable()
	//if err != nil {
	//    return err
	//}
	err = ipStruct.WriteIpTable()
	if err != nil {
		return err
	}

	wg.Done()

	return nil
}

// UpdateIpTableListClient updates IP tables (Default 3 hops) based on server information available
// on the ip tables
func UpdateIpTableListClient() error {
	// Get config information
	Config, err := config.ConfigInit(nil, nil)
	if err != nil {
		return err
	}

	// Ensure that the IP Table has Node pingable
	err = p2p.LocalSpeedTestIpTable()
	if err != nil {
		return err
	}

	// IP addresses to not append to struct due to
	// duplication

	Addresses, err := p2p.ReadIpTable()
	var DoNotRead p2p.IpAddresses

	currentIPV4, err := p2p.CurrentPublicIP()
	if err != nil {
		return err
	}

	var w sync.WaitGroup

	// Run loop 2 times
	for i := 0; i < 2; i++ {
		// Gets information from IP table
		Addresses, err = p2p.ReadIpTable()
		if err != nil {
			return err
		}

		//DoNotRead.IpAddress = append(DoNotRead.IpAddress, PublicIP)

		// Updates IP table based on server IP table
		for j := range Addresses.IpAddress {

			Exists := false
			// If the address is local then add to the local list
			if currentIPV4 == Addresses.IpAddress[j].Ipv4 && Addresses.IpAddress[j].ServerPort == Config.ServerPort {
				Exists = true
			}

			// Check if IP addresses is there in the struct DoNotRead
			for k := range DoNotRead.IpAddress {
				if (DoNotRead.IpAddress[k].Ipv4 == Addresses.IpAddress[j].Ipv4 && DoNotRead.IpAddress[k].ServerPort == Addresses.IpAddress[j].ServerPort) || (DoNotRead.IpAddress[k].Ipv6 != "" && Addresses.IpAddress[j].Ipv6 == DoNotRead.IpAddress[k].Ipv6) {
					Exists = true
					break
				}
			}

			// If the struct exists then continues
			if Exists {
				continue
			}

			w.Add(1)
			if Addresses.IpAddress[j].Ipv6 != "" {
				go UpdateIpTable(Addresses.IpAddress[j].Ipv6, Addresses.IpAddress[j].ServerPort, &w)
			} else if Addresses.IpAddress[j].Ipv4 != currentIPV4 {
				go UpdateIpTable(Addresses.IpAddress[j].Ipv4, Addresses.IpAddress[j].ServerPort, &w)
			}
			w.Wait()

			//Appends server1 IP address to variable DoNotRead
			DoNotRead.IpAddress = append(DoNotRead.IpAddress, Addresses.IpAddress[j])
		}
	}

	return nil
}

func RemoveOfflineNodes() error {
	// Ensure that the IP Table has Node pingable
	err := p2p.LocalSpeedTestIpTable()
	if err != nil {
		return err
	}
	return nil
}

// SendPostRequest Sends a file as a
//POST request.
// Reference (https://stackoverflow.com/questions/51234464/upload-a-file-with-post-request-golang)
//func SendPostRequest (url string, filename string, filetype string) ([]byte,error) {
//	file, err := os.Open(filename)
//	if err != nil {
//		return nil,err
//	}
//	defer file.Close()
//
//
//	body := &bytes.Buffer{}
//	writer := multipart.NewWriter(body)
//	part, err := writer.CreateFormFile(filetype, filepath.Base(file.Name()))
//
//	if err != nil {
//		return nil,err
//	}
//
//	io.Copy(part, file)
//	writer.Close()
//	request, err := http.NewRequest("POST", url, body)
//
//	if err != nil {
//		return nil,err
//	}
//
//	request.Header.Add("Content-Type", writer.FormDataContentType())
//	client := &http.Client{}
//
//	response, err := client.Do(request)
//
//	if err != nil {
//		return nil,err
//	}
//	defer response.Body.Close()
//
//	content, err := ioutil.ReadAll(response.Body)
//
//	if err != nil {
//		return nil,err
//	}
//
//	return content, nil
//}

func UploadMultipartFile(client http.Client, uri, key, path string) ([]byte, error) {
	body, writer := io.Pipe()

	req, err := http.NewRequest(http.MethodPost, uri, body)
	if err != nil {
		return nil, err
	}

	mwriter := multipart.NewWriter(writer)
	req.Header.Add("Content-Type", mwriter.FormDataContentType())

	errchan := make(chan error)

	go func() {
		defer close(errchan)
		defer writer.Close()
		defer mwriter.Close()

		w, err := mwriter.CreateFormFile(key, path)
		if err != nil {
			errchan <- err
			return
		}

		in, err := os.Open(path)
		if err != nil {
			errchan <- err
			return
		}
		defer in.Close()

		if written, err := io.Copy(w, in); err != nil {
			errchan <- fmt.Errorf("error copying %s (%d bytes written): %v", path, written, err)
			return
		}

		if err := mwriter.Close(); err != nil {
			errchan <- err
			return
		}
	}()

	resp, err := client.Do(req)
	if err != nil {
		return nil, err
	}

	content, err := ioutil.ReadAll(resp.Body)

	if err != nil {
		return nil, err
	}

	return content, nil

}
