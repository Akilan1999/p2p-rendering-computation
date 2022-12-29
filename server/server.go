package server

import (
	"encoding/json"
	"fmt"
	"git.sr.ht/~akilan1999/p2p-rendering-computation/config"
	"git.sr.ht/~akilan1999/p2p-rendering-computation/p2p"
	"git.sr.ht/~akilan1999/p2p-rendering-computation/p2p/frp"
	"git.sr.ht/~akilan1999/p2p-rendering-computation/server/docker"
	"github.com/gin-gonic/gin"
	"io/ioutil"
	"net/http"
)

func Server() error {
	r := gin.Default()

	// update IPTable with new port and ip address and update ip table
	var ProxyIpAddr p2p.IpAddress

	// Gets default information of the server
	r.GET("/server_info", func(c *gin.Context) {
		c.JSON(http.StatusOK, ServerInfo())
	})

	// Speed test with 50 mbps
	r.GET("/50", func(c *gin.Context) {
		// Get Path from config
		config, err := config.ConfigInit()
		if err != nil {
			c.String(http.StatusOK, fmt.Sprint(err))
		}
		c.File(config.SpeedTestFile)
	})

	// Route build to do a speed test
	r.GET("/upload", func(c *gin.Context) {
		file, _ := c.FormFile("file")

		// Upload the file to specific dst.
		// c.SaveUploadedFile(file, dst)

		c.String(http.StatusOK, fmt.Sprintf("'%s' uploaded!", file.Filename))
	})

	//Gets Ip Table from server node
	r.POST("/IpTable", func(c *gin.Context) {
		// Getting IPV4 address of client
		var ClientHost p2p.IpAddress

		if p2p.Ip4or6(c.ClientIP()) == "version 6" {
			ClientHost.Ipv6 = c.ClientIP()
		} else {
			ClientHost.Ipv4 = c.ClientIP()
		}

		// Variable to store IP table information
		var IPTable p2p.IpAddresses

		// Receive file from POST request
		body, err := c.FormFile("json")
		if err != nil {
			c.String(http.StatusOK, fmt.Sprint(err))
		}

		// Open file
		open, err := body.Open()
		if err != nil {
			c.String(http.StatusOK, fmt.Sprint(err))
		}

		// Open received file
		file, err := ioutil.ReadAll(open)
		if err != nil {
			c.String(http.StatusOK, fmt.Sprint(err))
		}

		json.Unmarshal(file, &IPTable)

		//Add Client IP address to IPTable struct
		IPTable.IpAddress = append(IPTable.IpAddress, ClientHost)

		// Runs speed test to return only servers in the IP table pingable
		err = IPTable.SpeedTestUpdatedIPTable()
		if err != nil {
			c.String(http.StatusOK, fmt.Sprint(err))
		}

		// Reads IP addresses from ip table
		IpAddresses, err := p2p.ReadIpTable()
		if err != nil {
			c.String(http.StatusOK, fmt.Sprint(err))
		}

		c.JSON(http.StatusOK, IpAddresses)
	})

	// Starts docker container in server
	r.GET("/startcontainer", func(c *gin.Context) {
		// Get Number of ports to open and whether to use GPU or not
		Ports := c.DefaultQuery("ports", "0")
		GPU := c.DefaultQuery("GPU", "false")
		ContainerName := c.DefaultQuery("ContainerName", "")
		var PortsInt int

		// Convert Get Request value to int
		fmt.Sscanf(Ports, "%d", &PortsInt)

		// Creates container and returns-back result to
		// access container
		resp, err := docker.BuildRunContainer(PortsInt, GPU, ContainerName)

		if ProxyIpAddr.Ipv4 != "" {
			err := frp.StartFRPCDockerContainer(ProxyIpAddr.Ipv4, ProxyIpAddr.ProxyPort, resp)
			if err != nil {
				c.String(http.StatusInternalServerError, fmt.Sprintf("error: %s", err))
			}
		}

		if err != nil {
			c.String(http.StatusInternalServerError, fmt.Sprintf("error: %s", err))
		}

		c.JSON(http.StatusOK, resp)
	})

	//Remove container
	r.GET("/RemoveContainer", func(c *gin.Context) {
		ID := c.DefaultQuery("id", "0")
		if err := docker.StopAndRemoveContainer(ID); err != nil {
			c.String(http.StatusInternalServerError, fmt.Sprintf("error: %s", err))
		}
		c.String(http.StatusOK, "success")
	})

	//Show images available
	r.GET("/ShowImages", func(c *gin.Context) {
		resp, err := docker.ViewAllContainers()
		if err != nil {
			c.String(http.StatusInternalServerError, fmt.Sprintf("error: %s", err))
		}
		c.JSON(http.StatusOK, resp)
	})

	//Get Server port based on the config file
	config, err := config.ConfigInit()
	if err != nil {
		return err
	}

	// If there is a proxy port specified
	// then starts the FRP server
	if config.FRPServerPort != "0" {
		go frp.StartFRPProxyFromConfig()
	}

	// TODO check if IPV6 or Proxy port is specified
	// if not update current entry as proxy address
	// with appropriate port on IP Table
	if config.BehindNAT == "True" {
		table, err := p2p.ReadIpTable()
		if err != nil {
			return err
		}

		var lowestLatency int64
		// random large number
		lowestLatency = 10000000
		var lowestLatencyIpAddress p2p.IpAddress
		for i, _ := range table.IpAddress {
			// Checks if the ping is the lowest and if the following node is acting as a proxy
			if table.IpAddress[i].Latency.Milliseconds() < lowestLatency && table.IpAddress[i].ProxyPort != "" {
				lowestLatency = table.IpAddress[i].Latency.Milliseconds()
				lowestLatencyIpAddress = table.IpAddress[i]
			}
		}

		// If there is an identified node
		if lowestLatency != 10000000 {
			// Starts FRP as a client with
			proxyPort, err := frp.StartFRPClientForServer(lowestLatencyIpAddress.Ipv4, lowestLatencyIpAddress.ProxyPort, config.ServerPort)
			if err != nil {
				return err
			}

			// updating with the current proxy address
			ProxyIpAddr.Ipv4 = lowestLatencyIpAddress.Ipv4
			ProxyIpAddr.ServerPort = proxyPort
			ProxyIpAddr.ProxyPort = lowestLatencyIpAddress.ProxyPort

			// append the following to the ip table
			table.IpAddress = append(table.IpAddress, ProxyIpAddr)
			// write information back to the IP Table
			table.WriteIpTable()
			// update ip table
			go table.SpeedTest()
		}

	}

	// Run gin server on the specified port
	err = r.Run(":" + config.ServerPort)
	if err != nil {
		return err
	}

	return nil
}
