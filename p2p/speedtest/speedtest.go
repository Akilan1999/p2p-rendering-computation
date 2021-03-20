package main

import (
	"fmt"
	"github.com/showwin/speedtest-go/speedtest"
	"net/http"
	"strings"
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


func main() {
	SpeedTest()
}

func SpeedTest() {
	user, _ := speedtest.FetchUserInfo()

	serverList, _ := speedtest.FetchServerList(user)
	targets, _ := serverList.FindServer([]int{})

	for _, s := range targets {
		fmt.Print(strings.Split(s.URL, "/upload")[0] + "/latency.txt")
		s.PingTest()
		s.DownloadTest(false)
		s.UploadTest(false)

		fmt.Printf("Latency: %s, Download: %f, Upload: %f\n", s.Latency, s.DLSpeed, s.ULSpeed)
	}
}

// PingTest executes test to measure latency
// Function modified for custom URl
func (s *Server) PingTest() error {
	pingURL := strings.Split(s.URL, "/upload")[0] + "/latency.txt"
	fmt.Print(pingURL)

	l := time.Duration(100000000000) // 10sec
	for i := 0; i < 3; i++ {
		sTime := time.Now()
		resp, err := http.Get(pingURL)
		fTime := time.Now()
		if err != nil {
			return err
		}
		if fTime.Sub(sTime) < l {
			l = fTime.Sub(sTime)
		}
		resp.Body.Close()
	}

	s.Latency = time.Duration(int64(l.Nanoseconds() / 2))

	return nil
}
