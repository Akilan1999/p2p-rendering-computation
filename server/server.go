package server

import (
	b64 "encoding/base64"
	"encoding/json"
	"errors"
	"fmt"
	"io/ioutil"
	"net/http"
	"os/user"
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

type ReverseProxy struct {
	IPAddress string
	Port      string
}

// ReverseProxies Reverse to the map such as ReverseProxies[<domain nane>]ReverseProxy type
var ReverseProxies map[string]ReverseProxy

func Server() (*gin.Engine, error) {
	r := gin.Default()

	// "The make function allocates and initializes a hash map data
	//structure and returns a map value that points to it.
	//The specifics of that data structure are an implementation
	//detail of the runtime and are not specified by the language itself."
	// source: https://go.dev/blog/maps
	ReverseProxies = make(map[string]ReverseProxy)

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
		//var ClientHost p2p.IpAddress
		//
		//if p2p.Ip4or6(c.ClientIP()) == "version 6" {
		//	ClientHost.Ipv6 = c.ClientIP()
		//} else {
		//	ClientHost.Ipv4 = c.ClientIP()
		//}

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
		//IPTable.IpAddress = append(IPTable.IpAddress, ClientHost)

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
		DomainName := c.DefaultQuery("domain_name", "")
		url, _, err := MapPort(Ports, DomainName)
		if err != nil {
			c.String(http.StatusInternalServerError, fmt.Sprintf("error: %s", err))
		}

		c.String(http.StatusOK, url)
	})

	r.GET("/AddProxy", func(c *gin.Context) {
		DomainName := c.DefaultQuery("domain_name", "")
		ip_address := c.DefaultQuery("ip_address", "")
		Ports := c.DefaultQuery("port", "")

		if DomainName == "" || ip_address == "" || Ports == "" {
			c.String(http.StatusInternalServerError, fmt.Sprintf("All get parameters npt provided"+
				" do ensure domain_name, ip_Address and port no is provided"))
		}

		err := SaveRegistration(DomainName, ip_address+":"+Ports)
		if err != nil {
			c.String(http.StatusInternalServerError, fmt.Sprintf(err.Error()))
		}

		//_, ok := ReverseProxies[DomainName]
		//// To check if the subdomain entry exists
		//if ok {
		//	c.String(http.StatusInternalServerError, fmt.Sprintf("The domain entry already exists as a reverse"+
		//		" proxy entry"))
		//}
		//
		//// added proxy as a map entry
		//ReverseProxies[DomainName] = ReverseProxy{IPAddress: ip_address, Port: Ports}
		c.String(http.StatusOK, "Sucess")

	})

	//r.GET("/RemoveProxy", func(c *gin.Context) {
	//	DomainName := c.DefaultQuery("domain_name", "")
	//
	//	_, ok := ReverseProxies[DomainName]
	//	if !ok {
	//		c.String(http.StatusInternalServerError, fmt.Sprintf("Domain name does exist in entries of proxies"))
	//	} else {
	//		delete(ReverseProxies, DomainName)
	//	}
	//
	//})

	// If there is a proxy port specified
	// then starts the FRP server
	//if config.FRPServerPort != "0" {
	//	go frp.StartFRPProxyFromRandom()
	//}

	// Remove nodes currently not pingable
	clientIPTable.RemoveOfflineNodes()

	table, err := p2p.ReadIpTable()

	// TODO check if IPV6 or Proxy port is specified
	// if not update current entry as proxy address
	// with appropriate port on IP Table
	if config.BehindNAT == "True" {
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
			ProxyIpAddr.ProxyServer = "False"
			ProxyIpAddr.EscapeImplementation = "FRP"

			if config.BareMetal {
				_, SSHPort, err := MapPort("22", "")
				if err != nil {
					return nil, err
				}
				ProxyIpAddr.BareMetalSSHPort = SSHPort
			}

			//ProxyIpAddr.CustomInformationKey = p2p.GenerateHashSHA256(config.IPTableKey)
			// write information back to the IP Table
		}

	} else {
		ProxyIpAddr.Ipv4, err = p2p.CurrentPublicIP()
		if err != nil {
			fmt.Println(err)
		}
		ProxyIpAddr.ServerPort = config.ServerPort
		ProxyIpAddr.Name = config.MachineName
		ProxyIpAddr.NAT = "False"
		if config.ProxyPort != "" {
			ProxyIpAddr.ProxyServer = "True"
		}
		ProxyIpAddr.EscapeImplementation = ""
		if config.BareMetal {
			ProxyIpAddr.BareMetalSSHPort = "22"
		}

	}

	// Get machine username
	currentUser, err := user.Current()
	if err != nil {
		return nil, err
	}
	// Add username p2prc binary is being run under
	ProxyIpAddr.MachineUsername = currentUser.Username
	ProxyIpAddr.UnSafeMode = config.UnsafeMode
	// Adds the public key information to the IPTable
	// improving transmission of IPTables without the
	// entire public key is future work to be improved
	// in the P2PRC protocol level (Improving by adding
	// UDP with TCP reliability layer).
	ProxyIpAddr.PublicKey, err = config.GetPublicKey()

	if err != nil {
		return nil, err
	}

	// append the following to the ip table
	table.IpAddress = append(table.IpAddress, ProxyIpAddr)

	// Writing results to the IPTable
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

	if config.ProxyPort != "" {
		go ProxyRun(config.ProxyPort)
	}

	// Run gin server on the specified port
	go r.Run(":" + config.ServerPort)

	return r, nil
}

func MapPort(port string, domainName string) (string, string, error) {

	// if server address is provided to do call RESTAPI to remotely open port.
	//if serverAddress != "" {
	//	requestURL := fmt.Sprintf("http://%v/MAPPort?port=%v&domain_name=%v", serverAddress, port, domainName)
	//	req, err := http.NewRequest(http.MethodGet, requestURL, nil)
	//	if err != nil {
	//		return "", "", err
	//	}
	//
	//	res, err := http.DefaultClient.Do(req)
	//	if err != nil {
	//		return "", "", err
	//	}
	//
	//	resBody, err := io.ReadAll(res.Body)
	//	if err != nil {
	//		return "", "", err
	//	}
	//
	//	_, Exposedport, err := net.SplitHostPort(string(resBody))
	//	if err != nil {
	//		return "", "", err
	//	}
	//
	//	return string(resBody), Exposedport, nil
	//}

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
			// Filter based on nodes with proxy enabled
			if domainName != "" && table.IpAddress[i].ProxyServer == "True" {
				lowestLatency = table.IpAddress[i].Latency.Milliseconds()
				lowestLatencyIpAddress = table.IpAddress[i]
				continue
			}
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

		// Doing the proxy mapping for the domain name
		if domainName != "" {
			fmt.Println("http://" + lowestLatencyIpAddress.Ipv4 + ":" + lowestLatencyIpAddress.ServerPort + "/AddProxy?port=" + proxyPort + "&domain_name=" + domainName + "&ip_address=" + lowestLatencyIpAddress.Ipv4)
			URL := "http://" + lowestLatencyIpAddress.Ipv4 + ":" + lowestLatencyIpAddress.ServerPort + "/AddProxy?port=" + proxyPort + "&domain_name=" + domainName + "&ip_address=" + lowestLatencyIpAddress.Ipv4
			//} else {
			//	URL = "http://" + IP + ":" + serverPort + "/server_info"
			//}
			http.Get(URL)
			//SaveRegistration(domainName, lowestLatencyIpAddress.Ipv4+":"+proxyPort)
		}

		// updating with the current proxy address
		ProxyIpAddr.Ipv4 = lowestLatencyIpAddress.Ipv4
		ProxyIpAddr.ServerPort = proxyPort
		ProxyIpAddr.Name = config.MachineName
		ProxyIpAddr.NAT = "False"
		ProxyIpAddr.EscapeImplementation = "FRP"

		//ProxyIpAddr.CustomInformationKey = p2p.GenerateHashSHA256(config.IPTableKey)
	} else {
		return "", "", errors.New("proxy IP not found")
	}

	return ProxyIpAddr.Ipv4 + ":" + ProxyIpAddr.ServerPort, ProxyIpAddr.ServerPort, nil
}
