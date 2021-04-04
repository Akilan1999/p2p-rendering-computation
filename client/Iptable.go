package client

import (
	"fmt"
	"git.sr.ht/~akilan1999/p2p-rendering-computation/p2p"
	"net/url"
)

// Does the following to update it's IP table
func UpdateIpTable(IpAddress string) error {
	// Gets information from IP table
	Addresses, err := p2p.ReadIpTable()
	if err != nil {
		return err
	}

	u, err := url.Parse("http://" + IpAddress + ":8088/IpTable")
	if err != nil {
		panic(err)
	}

	params := url.Values{}
	params.Add("data", fmt.Sprint(Addresses))
	u.RawQuery = params.Encode()

	fmt.Print(u.RawQuery)

	return nil
}

//updates IP tables (Default 3 hops) based on server information avaliable
//on the ip tables
func UpdateIpTableListClient() error {
	// Ensure that the IP Table has Node pingable
	err := p2p.LocalSpeedTestIpTable()
	if err != nil {
		return err
	}

	// IP addresses to not read from
	var DoNotRead p2p.IpAddresses

	// Run loop 3 times
	for i := 0; i < 3; i++ {
		// Gets information from IP table
		Addresses, err := p2p.ReadIpTable()
		if err != nil {
			return err
		}

		// Updates IP table based on server IP table
		for j, _ := range Addresses.IpAddress {

			// Check if IP addresses is there in the struct DoNotRead
			Exists := false
			for k, _ := range DoNotRead.IpAddress {
				if DoNotRead.IpAddress[k].Ipv4 == Addresses.IpAddress[i].Ipv4 {
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
