package p2p

import (
	"bytes"
	"git.sr.ht/~akilan1999/p2p-rendering-computation/config"
	"io"
	"io/ioutil"
	"log"
	"mime/multipart"
	"net/http"
	"os"
	"time"
)

//var dlSizes = [...]int{350, 500, 750, 1000, 1500, 2000, 2500, 3000, 3500, 4000}
//var ulSizes = [...]int{100, 300, 500, 800, 1000, 1500, 2500, 3000, 3500, 4000} //kB
var client = http.Client{}

// DownloadTest executes the test to measure download speed
//func (s *IpAddress) DownloadTest(savingMode bool) error {
//	dlURL := "http://" + s.Ipv4 + ":8088/server_info"
//	eg := errgroup.Group{}
//
//	// Warming up
//	sTime := time.Now()
//	for i := 0; i < 2; i++ {
//		eg.Go(func() error {
//			return dlWarmUp("http://" + s.Ipv4 + ":8088/server_info")
//		})
//	}
//	if err := eg.Wait(); err != nil {
//		return err
//	}
//	fTime := time.Now()
//	// 1.125MB for each request (750 * 750 * 2)
//	wuSpeed := 1.125 * 8 * 2 / fTime.Sub(sTime.Add(s.Latency)).Seconds()
//
//	// Decide workload by warm up speed
//	workload := 0
//	weight := 0
//	skip := false
//	if savingMode {
//		workload = 6
//		weight = 3
//	} else if 10.0 < wuSpeed {
//		workload = 16
//		weight = 4
//	} else if 4.0 < wuSpeed {
//		workload = 8
//		weight = 4
//	} else if 2.5 < wuSpeed {
//		workload = 4
//		weight = 4
//	} else {
//		skip = true
//	}
//
//	// Main speedtest
//	dlSpeed := wuSpeed
//	if skip == false {
//		sTime = time.Now()
//		for i := 0; i < workload; i++ {
//			eg.Go(func() error {
//				return downloadRequest(dlURL, weight)
//			})
//		}
//		if err := eg.Wait(); err != nil {
//			return err
//		}
//		fTime = time.Now()
//
//		reqMB := dlSizes[weight] * dlSizes[weight] * 2 / 1000 / 1000
//		dlSpeed = float64(reqMB) * 8 * float64(workload) / fTime.Sub(sTime).Seconds()
//	}
//
//	s.Download = dlSpeed
//	return nil
//}

// Download Speed
func (s *IpAddress)DownloadSpeed() error {
	start := time.Now()
	resp, err := client.Get("http://" + s.Ipv4 + ":8088/50")
	if err != nil {
		return err
	}
	defer resp.Body.Close()
	ioutil.ReadAll(resp.Body)
	t := time.Since(start)
	//fmt.Println(s.Seconds())
	// size * time (seconds)
	s.Download = (50/t.Seconds())*8
    return nil
}

func (s *IpAddress)UploadSpeed() error {
	start := time.Now()

	// Get upload file path from config file
	// Get Path from config
	config, err := config.ConfigInit()
	if err != nil {
		return err
	}

	b, w := createMultipartFormData("file",config.SpeedTestFile)

	req, err := http.NewRequest("GET", "http://" + s.Ipv4 + ":8088/upload", &b)
	if err != nil {
		return err
	}
	// Don't forget to set the content type, this will contain the boundary.
	req.Header.Set("Content-Type", w.FormDataContentType())
	defer req.Body.Close()
	ioutil.ReadAll(req.Body)
	t := time.Since(start)
	//fmt.Println(s.Seconds())
	// size * time (seconds)
	s.Upload = (50/t.Seconds())*8
	return nil
}

//Upload helper function for uploading
//(https://stackoverflow.com/questions/20205796/post-data-using-the-content-type-multipart-form-data
func createMultipartFormData(fieldName, fileName string) (bytes.Buffer, *multipart.Writer) {
	var b bytes.Buffer
	var err error
	w := multipart.NewWriter(&b)
	var fw io.Writer
	file := mustOpen(fileName)
	if fw, err = w.CreateFormFile(fieldName, file.Name()); err != nil {
		log.Fatalf("Error creating writer: %v", err)
	}
	if _, err = io.Copy(fw, file); err != nil {
		log.Fatalf("Error with io.Copy: %v", err)
		//t.Errorf("Error with io.Copy: %v", err)
	}
	w.Close()
	return b, w
}

func mustOpen(f string) *os.File {
	r, err := os.Open(f)
	if err != nil {
		log.Fatalf("Error with mustOpen: %v",err)
	}
	return r
}

// PingTest executes test to measure latency
func (s *IpAddress) PingTest() error {
	//pingURL := strings.Split(s.URL, "/upload")[0] + "/latency.txt"
	  pingURL := "http://" + s.Ipv4 + ":8088/server_info"
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

