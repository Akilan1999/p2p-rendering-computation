package client

import (
	"encoding/json"
	"errors"
	"fmt"
	"git.sr.ht/~akilan1999/p2p-rendering-computation/config"
	"git.sr.ht/~akilan1999/p2p-rendering-computation/p2p"
	"io"
	"io/ioutil"
	"mime/multipart"
	"net"
	"net/http"
	"os"
)

type IP struct {
	Query string
}

// UpdateIpTable Does the following to update it's IP table
func UpdateIpTable(IpAddress string) error {

	config, err := config.ConfigInit()
	if err != nil {
		return err
	}

	client := http.Client{}

	var resp []byte

	version := ip4or6(IpAddress)
	if version == "version 6" {
		resp, err = UploadMultipartFile(client,"http://["+IpAddress+"]:8088/IpTable","json",config.IPTable)
		if err != nil {
			return err
		}
	} else {
		resp, err = UploadMultipartFile(client,"http://"+IpAddress+":8088/IpTable","json",config.IPTable)
		if err != nil {
			return err
		}
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
	var DoNotRead p2p.IpAddresses

	// Run loop 3 times
	for i := 0; i < 3; i++ {
		// Gets information from IP table
		Addresses, err = p2p.ReadIpTable()
		if err != nil {
			return err
		}

		// Appending current machine public IP address as should not be there in IP Table
		var PublicIP p2p.IpAddress

		ipv6, err := GetCurrentIPV6()
		if err != nil {
			return err
		}

		ip, err := CurrentPublicIP()
		if err != nil {
			return err
		}
		PublicIP.Ipv4 = ip
		PublicIP.Ipv6 = ipv6

		DoNotRead.IpAddress = append(DoNotRead.IpAddress, PublicIP)

		// Updates IP table based on server IP table
		for j := range Addresses.IpAddress {

			Exists := false

			if PublicIP.Ipv4 == Addresses.IpAddress[j].Ipv4 {
				Exists = true
			}

			// Check if IP addresses is there in the struct DoNotRead
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

	// Removing duplicates in the IP table
	if err := p2p.RemoveDuplicates(); err != nil {
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
	content, err := ioutil.ReadAll(resp.Body)

	if err != nil {
		return nil, err
	}

	return content, nil

}

// CurrentPublicIP Get Current Public IP address
func CurrentPublicIP() (string,error) {
	req, err := http.Get("http://ip-api.com/json/")
	if err != nil {
		return "",err
	}
	defer req.Body.Close()

	body, err := ioutil.ReadAll(req.Body)
	if err != nil {
		return "",err
	}

	var ip IP
	json.Unmarshal(body, &ip)

	return ip.Query, nil
}

// GetCurrentIPV6 gets the current IPV6	address based on the interface
// specified in the config file
func GetCurrentIPV6()(string,error){
		Config, err := config.ConfigInit()
		if err != nil {
			return "",err
		}
		byNameInterface, err := net.InterfaceByName(Config.NetworkInterface)
		if err != nil {
			return "",err
		}
		addresses, err := byNameInterface.Addrs()
		if err != nil {
			return "",err
		}
		if addresses[1].String() == "" {
			return "",errors.New("IPV6 address not detected")
		}
		IP,_,err := net.ParseCIDR(addresses[1].String())
	    if err != nil {
		   return "",err
	    }

		return IP.String(), nil
}

//Helper function to check if the IP address is IPV4 or
//IPV6 (https://socketloop.com/tutorials/golang-check-if-ip-address-is-version-4-or-6)
func ip4or6(s string) string {
	for i := 0; i < len(s); i++ {
		switch s[i] {
		case '.':
			return "version 4"
		case ':':
			return "version 6"
		}
	}
	return "unknown"

}