package p2p

// SpeedTest Runs a speed test and does updates IP tables accordingly
func (ip *IpAddresses) SpeedTest() error {

    //temp variable to store elements IP addresses and other information
    // of IP addresses that are pingable
    var ActiveIP IpAddresses

    // Index to remove from struct
    for _, value := range ip.IpAddress {

        var err error
        //if len(ip.IpAddress) == 1 {
        //	i = 0
        //}

        // Ping Test
        err = value.PingTest()

        if err != nil {
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

        ActiveIP.IpAddress = append(ActiveIP.IpAddress, value)

    }

    ip.IpAddress = ActiveIP.IpAddress

    err := ip.WriteIpTable()
    if err != nil {
        return err
    }

    return nil
}

// SpeedTestUpdatedIPTable Called when ip tables from httpclient/server is also passed on
func (ip *IpAddresses) SpeedTestUpdatedIPTable() error {
    targets, err := ReadIpTable()
    if err != nil {
        return err
    }

    // To ensure struct has no duplicates IP addresses
    //DoNotRead := targets

    // Appends all IP addresses
    for i, _ := range targets.IpAddress {

        // To ensure that there are no duplicate IP addresses
        //Exists := false
        //for k := range ip.IpAddress {
        //	// Checks if both the IPV4 addresses are the same or the IPV6 address is not
        //	// an empty string and IPV6 address are the same
        //	if ip.IpAddress[k].Ipv4 == targets.IpAddress[i].Ipv4 || (targets.IpAddress[i].Ipv6 != "" && ip.IpAddress[k].Ipv6 == targets.IpAddress[i].Ipv6) {
        //		Exists = true
        //		break
        //	}
        //}
        //
        //// If the struct exists then continues
        //if Exists {
        //	continue
        //}

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
