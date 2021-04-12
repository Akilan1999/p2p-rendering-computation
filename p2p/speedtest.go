package p2p

// SpeedTest Runs a speed test and does updates IP tables accordingly
func (ip *IpAddresses)SpeedTest() error{

	for i, _ := range ip.IpAddress {
		// Ping Test
		err := ip.IpAddress[i].PingTest()
		if err != nil {
			// Remove IP address of element not pingable 
			ip.IpAddress = append(ip.IpAddress[:i], ip.IpAddress[i+1:]...)
			// Proceed to next element in the array  
			continue
		}

		//Upload Speed Test
		err = ip.IpAddress[i].UploadSpeed()
		if err != nil {
			return err
		}

		err = ip.IpAddress[i].DownloadSpeed()
		if err != nil {
			return err
		}
	}

	err := ip.WriteIpTable()
	if err != nil {
		return err
	}

	return nil
}

// SpeedTestUpdatedIPTable Called when ip tables from client/server is also passed on
func (ip *IpAddresses)SpeedTestUpdatedIPTable() error{
	targets, err := ReadIpTable()
	if err != nil {
		return err
	}

	// To ensure struct has no duplicates IP addresses
	DoNotRead := targets

    // Appends all IP addresses
	for i, _ := range targets.IpAddress {

		Exists := false
		for k := range DoNotRead.IpAddress {
			if DoNotRead.IpAddress[k].Ipv4 == targets.IpAddress[i].Ipv4 {
				Exists = true
				break
			}
		}

		// If the struct exists then continues
		if Exists {
			continue
		}

		ip.IpAddress = append(ip.IpAddress, targets.IpAddress[i])
	}

	err = ip.SpeedTest()

	if err != nil {
		return err
	}

	return nil
}

// LocalSpeedTestIpTable Runs speed test in iptables locally only
func LocalSpeedTestIpTable() error {
	targets, err := ReadIpTable()
	if err != nil {
		return err
	}

	err = targets.SpeedTest()
	if err != nil {
		return err
	}

	return nil
}

