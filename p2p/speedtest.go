package p2p

import (
	"fmt"
)

func SpeedTest() error{

	targets, err := ReadIpTable()

	if err != nil {
		return err
	}

	for _, s := range targets.IpAddress {
		
		// Ping Test
		err = s.PingTest()
		if err != nil {
			return err
		}

		//Upload Speed Test
		err = s.UploadSpeed()
		if err != nil {
			return err
		}

		err = s.DownloadSpeed()
		if err != nil {
			return err
		}

		//s.DownloadTest(false)
		//s.UploadTest(false)
		fmt.Println(s.Upload)
		//fmt.Printf("Latency: %s, Download: %f, Upload: %f\n", s.Latency, s.DLSpeed, s.ULSpeed)
	}

	return nil
}


