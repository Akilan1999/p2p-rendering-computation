package server

import (
	b64 "encoding/base64"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"net/http"
	"strconv"
	"time"

	"github.com/Akilan1999/p2p-rendering-computation/client/clientIPTable"
	"github.com/Akilan1999/p2p-rendering-computation/config"
	"github.com/Akilan1999/p2p-rendering-computation/p2p"
	"github.com/Akilan1999/p2p-rendering-computation/p2p/frp"
	"github.com/Akilan1999/p2p-rendering-computation/server/docker"
	"github.com/gin-gonic/gin"
	"github.com/phayes/freeport"
)

func Server() (*gin.Engine, error) {
	r := gin.Default()

	//Get Server port based on the config file
	config, err := config.ConfigInit(nil, nil)
	if err != nil {
		return nil, err
	}

	// update IPTable with new port and ip address and update ip table
	var ProxyIpAddr p2p.IpAddress
	var lowestLatencyIpAddress p2p.IpAddress

	// Gets default information of the server
	r.GET("/server_info", func(c *gin.Context) {
		c.JSON(http.StatusOK, ServerInfo())
	})

	// Speed test with 50 mbps
	r.GET("/50", func(c *gin.Context) {
		// Get Path from config
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
		BaseImage := c.DefaultQuery("BaseImage", "")
		PublicKey := c.DefaultQuery("PublicKey", "")
		var PortsInt int

		if PublicKey == "" {
			c.String(http.StatusInternalServerError, fmt.Sprintf("error: %s", "Publickey not passed"))
		}

		PublicKeyDecoded, err := b64.StdEncoding.DecodeString(PublicKey)
		if err != nil {
			c.String(http.StatusInternalServerError, fmt.Sprintf("error: %s", err))
		}

		// Convert Get Request value to int
		fmt.Sscanf(Ports, "%d", &PortsInt)

		fmt.Println(string(PublicKeyDecoded[:]))

		// Creates container and returns-back result to
		// access container
		resp, err := docker.BuildRunContainer(PortsInt, GPU, ContainerName, BaseImage, string(PublicKeyDecoded[:]))

		if err != nil {
			c.String(http.StatusInternalServerError, fmt.Sprintf("error: %s", err))
		}

		// Ensures that FRP is triggered only if a proxy address is provided
		if ProxyIpAddr.Ipv4 != "" && c.Request.Host != "localhost:"+config.ServerPort && c.Request.Host != "0.0.0.0:"+config.ServerPort {
			resp, err = frp.StartFRPCDockerContainer(ProxyIpAddr.Ipv4, lowestLatencyIpAddress.ServerPort, resp)
			if err != nil {
				c.String(http.StatusInternalServerError, fmt.Sprintf("error: %s", err))
			}
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

	r.GET("/FreePort", func(c *gin.Context) {
		openports, err := freeport.GetFreePorts(1)

		if err != nil {
			c.String(http.StatusInternalServerError, fmt.Sprintf("error: %s", err))
		}

		c.String(http.StatusOK, strconv.Itoa(openports[0]))
	})

	// Request for port no from Server with address
	r.GET("/FRPPort", func(c *gin.Context) {
		port, err := frp.StartFRPProxyFromRandom()
		if err != nil {
			c.String(http.StatusInternalServerError, fmt.Sprintf("error: %s", err))
		}

		c.String(http.StatusOK, strconv.Itoa(port))
	})

	r.GET("/MAPPort", func(c *gin.Context) {
		Ports := c.DefaultQuery("port", "0")
		url, _, err := MapPort(Ports)
		if err != nil {
			c.String(http.StatusInternalServerError, fmt.Sprintf("error: %s", err))
		}

		c.String(http.StatusOK, url)
	})

	// If there is a proxy port specified
	// then starts the FRP server
	//if config.FRPServerPort != "0" {
	//	go frp.StartFRPProxyFromRandom()
	//}

	// TODO check if IPV6 or Proxy port is specified
	// if not update current entry as proxy address
	// with appropriate port on IP Table
	if config.BehindNAT == "True" {
		// Remove nodes currently not pingable
		clientIPTable.RemoveOfflineNodes()

		table, err := p2p.ReadIpTable()
		if err != nil {
			return nil, err
		}

		var lowestLatency int64
		// random large number
		lowestLatency = 10000000

		for i, _ := range table.IpAddress {
			// Checks if the ping is the lowest and if the following node is acting as a proxy
			//if table.IpAddress[i].Latency.Milliseconds() < lowestLatency && table.IpAddress[i].ProxyPort != "" {
			if table.IpAddress[i].Latency.Milliseconds() < lowestLatency && table.IpAddress[i].NAT != "" {
				lowestLatency = table.IpAddress[i].Latency.Milliseconds()
				lowestLatencyIpAddress = table.IpAddress[i]
			}
		}

		// If there is an identified node
		if lowestLatency != 10000000 {
			serverPort, err := frp.GetFRPServerPort("http://" + lowestLatencyIpAddress.Ipv4 + ":" + lowestLatencyIpAddress.ServerPort)
			if err != nil {
				return nil, err
			}
			// Create 3 second delay to allow FRP server to start
			time.Sleep(1 * time.Second)
			// Starts FRP as a client with
			proxyPort, err := frp.StartFRPClientForServer(lowestLatencyIpAddress.Ipv4, serverPort, config.ServerPort, "")
			if err != nil {
				return nil, err
			}

			// updating with the current proxy address
			ProxyIpAddr.Ipv4 = lowestLatencyIpAddress.Ipv4
			ProxyIpAddr.ServerPort = proxyPort
			ProxyIpAddr.Name = config.MachineName
			ProxyIpAddr.NAT = "False"
			ProxyIpAddr.EscapeImplementation = "FRP"

			// Sorry Jan it's a string
			// Yes I could convert it
			// to a boolean operator
			// But I am way too tired
			if config.BareMetal == "True" {
				_, SSHPort, err := MapPort("22")
				if err != nil {
					return nil, err
				}
				ProxyIpAddr.BareMetalSSHPort = SSHPort
			}

			//ProxyIpAddr.CustomInformationKey = p2p.GenerateHashSHA256(config.IPTableKey)

			// append the following to the ip table
			table.IpAddress = append(table.IpAddress, ProxyIpAddr)
			// write information back to the IP Table
			err = table.WriteIpTable()
			if err != nil {
				return nil, err
			}
			// update ip table
			go func() error {
				err := clientIPTable.UpdateIpTableListClient()
				if err != nil {
					fmt.Println(err)
					return err
				}
				return nil
			}()
		}

	}

	// Run gin server on the specified port
	go r.Run(":" + config.ServerPort)

	return r, nil
}

func MapPort(port string) (string, string, error) {
	//Get Server port based on the config file
	config, err := config.ConfigInit(nil, nil)
	if err != nil {
		return "", "", err
	}

	// update IPTable with new port and ip address and update ip table
	var ProxyIpAddr p2p.IpAddress
	var lowestLatencyIpAddress p2p.IpAddress

	clientIPTable.RemoveOfflineNodes()

	table, err := p2p.ReadIpTable()
	if err != nil {
		return "", "", err
	}

	var lowestLatency int64
	// random large number
	lowestLatency = 10000000

	for i, _ := range table.IpAddress {
		// Checks if the ping is the lowest and if the following node is acting as a proxy
		//if table.IpAddress[i].Latency.Milliseconds() < lowestLatency && table.IpAddress[i].ProxyPort != "" {
		if table.IpAddress[i].Latency.Milliseconds() < lowestLatency && table.IpAddress[i].NAT != "" {
			lowestLatency = table.IpAddress[i].Latency.Milliseconds()
			lowestLatencyIpAddress = table.IpAddress[i]
		}
	}

	// If there is an identified node
	if lowestLatency != 10000000 {
		serverPort, err := frp.GetFRPServerPort("http://" + lowestLatencyIpAddress.Ipv4 + ":" + lowestLatencyIpAddress.ServerPort)
		if err != nil {
			return "", "", err
		}
		// Create 3 second delay to allow FRP server to start
		time.Sleep(1 * time.Second)
		// Starts FRP as a client with
		proxyPort, err := frp.StartFRPClientForServer(lowestLatencyIpAddress.Ipv4, serverPort, port, "")
		if err != nil {
			return "", "", err
		}

		// updating with the current proxy address
		ProxyIpAddr.Ipv4 = lowestLatencyIpAddress.Ipv4
		ProxyIpAddr.ServerPort = proxyPort
		ProxyIpAddr.Name = config.MachineName
		ProxyIpAddr.NAT = "False"
		ProxyIpAddr.EscapeImplementation = "FRP"
		//ProxyIpAddr.CustomInformationKey = p2p.GenerateHashSHA256(config.IPTableKey)
	}

	return ProxyIpAddr.Ipv4 + ":" + ProxyIpAddr.ServerPort, ProxyIpAddr.ServerPort, nil
}
