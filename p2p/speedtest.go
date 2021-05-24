package p2p

// SpeedTest Runs a speed test and does updates IP tables accordingly
func (ip *IpAddresses)SpeedTest() error{
    
	// Index to remove from struct 
	var RemoveIndex []int
	for i, value := range ip.IpAddress {

		var err error
		//if len(ip.IpAddress) == 1 {
		//	i = 0
		//}

		// Ping Test
		err = value.PingTest()

		if err != nil {
			RemoveIndex = append(RemoveIndex, i)
			// Record index to remove 
			//ip.IpAddress = append(ip.IpAddress[:i], ip.IpAddress[i+1:]...)
			// Proceed to next element in the array  
			continue
		}

		//Upload Speed Test
		//err = value.UploadSpeed()
		//if err != nil {
		//	return err
		//}
		//
		//err = value.DownloadSpeed()
		//if err != nil {
		//	return err
		//}

		//Set value to the list
		ip.IpAddress[i] = value

	}
	// Remove element from struct 
	for _, index := range RemoveIndex {
		// If there is only 1 element and that has to be
		// removed
		if len(ip.IpAddress) == 1 {
		   ip.IpAddress = nil
		   break
		}
		ip.IpAddress = append(ip.IpAddress[:index], ip.IpAddress[index+1:]...)
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
	//DoNotRead := targets

    // Appends all IP addresses
	for i, _ := range targets.IpAddress {

		// To ensure that there are no duplicate IP addresses
		Exists := false
		for k := range ip.IpAddress {
			if ip.IpAddress[k].Ipv4 == targets.IpAddress[i].Ipv4 {
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

// Helper function to remove element from an array of a struct
//func remove(s []IpAddress, i int) []IpAddress {
//	s[len(s)-1], s[i] = s[i], s[len(s)-1]
//	return s[:len(s)-1]
//}
