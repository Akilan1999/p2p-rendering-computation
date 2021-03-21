package p2p

import (
	"fmt"
	"time"
)

type Server struct {
	URL      string `xml:"url,attr"`
	Lat      string `xml:"lat,attr"`
	Lon      string `xml:"lon,attr"`
	Name     string `xml:"name,attr"`
	Country  string `xml:"country,attr"`
	Sponsor  string `xml:"sponsor,attr"`
	ID       string `xml:"id,attr"`
	URL2     string `xml:"url2,attr"`
	Host     string `xml:"host,attr"`
	Distance float64
	Latency  time.Duration
	DLSpeed  float64
	ULSpeed  float64
}


func SpeedTest() error{

	targets, err := ReadIpTable()

	if err != nil {
		return err
	}
	//serverList, _ := speedtest.FetchServerList(user)
	//targets, _ := serverList.FindServer([]int{})

	for _, s := range targets.IpAddress {
		//fmt.Print(strings.Split(i., "/upload")[0] + "/latency.txt")
		// Ping Test
		err = s.PingTest()
		if err != nil {
			return err
		}

		//Upload Speed Test
		err = s.UploadTest(false)
		if err != nil {
			return err
		}

		err = s.DownloadSpeed()
		if err != nil {
			return err
		}

		//s.DownloadTest(false)
		//s.UploadTest(false)
		fmt.Println(s.Download)
		//fmt.Printf("Latency: %s, Download: %f, Upload: %f\n", s.Latency, s.DLSpeed, s.ULSpeed)
	}

	return nil
}


