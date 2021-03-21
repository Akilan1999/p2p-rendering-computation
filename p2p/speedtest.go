package p2p

import "fmt"

func SpeedTest() error{

	targets, err := ReadIpTable()

	if err != nil {
		return err
	}

	for i, _ := range targets.IpAddress {

		// Ping Test
		err = targets.IpAddress[i].PingTest()
		if err != nil {
			return err
		}

		//Upload Speed Test
		err = targets.IpAddress[i].UploadSpeed()
		if err != nil {
			return err
		}

		err = targets.IpAddress[i].DownloadSpeed()
		if err != nil {
			return err
		}
		fmt.Println(targets.IpAddress[i].Latency)
	}

	err = targets.WriteIpTable()
	if err != nil {
		return err
	}

	return nil
}


