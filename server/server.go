package server

import (
	"fmt"
	"git.sr.ht/~akilan1999/p2p-rendering-computation/p2p"
	"git.sr.ht/~akilan1999/p2p-rendering-computation/server/docker"
	"github.com/gin-gonic/gin"
	"net/http"
	//"fmt"
)

func Server() {
	r := gin.Default()

	// Gets default information of the server
	r.GET("/server_info", func(c *gin.Context) {
		c.JSON(http.StatusOK, ServerInfo())
	})

	// Speed test with 50 mbps
	r.GET("/50", func(c *gin.Context){
		c.File("/etc/p2p-rendering/50.bin")
	})

	// Route build to do a speed test
	r.GET("/upload", func(c *gin.Context) {
		file, _ := c.FormFile("file")

		// Upload the file to specific dst.
		// c.SaveUploadedFile(file, dst)

		c.String(http.StatusOK, fmt.Sprintf("'%s' uploaded!", file.Filename))
	})

	//Gets Ip Table from server node
	r.GET("/IpTable", func(c *gin.Context) {

		//jsonData, err := ioutil.ReadAll(c.Request.Body)
		//if err != nil {
		//	c.String(http.StatusOK, fmt.Sprint(err))
		//}

		// Runs speed test to return only servers in the IP table pingable
		err := p2p.LocalSpeedTestIpTable()
		if err != nil {
			c.String(http.StatusOK, fmt.Sprint(err))
		}

		// Reads IP addresses from ip table
		IpAddresses,err := p2p.ReadIpTable()
		if err != nil {
			c.String(http.StatusOK, fmt.Sprint(err))
		}

		c.JSON(http.StatusOK, IpAddresses)
	})

    // Starts docker container in server
	r.GET("/startcontainer", func(c *gin.Context) {

		resp, err := docker.BuildRunContainer()

		if err != nil {
			c.String(http.StatusInternalServerError, fmt.Sprintf("error: %s", err))
		}

		c.JSON(http.StatusOK, resp)
	})

	// Future feature
	/*r.GET("/create_vm/:virtualization", func(c *gin.Context) {
		virtualization := c.Param("virtualization")
		// Runs based on Preallocated VM size
		if virtualization == "docker" {
		  sshinfo,err := docker.RunVM()
		 if err != nil {
			c.String(http.StatusInternalServerError, fmt.Sprintf("error: %s", err))
		 }
		 if sshinfo != nil {
            c.JSON(http.StatusOK, sshinfo)
		 }

		} else {
			c.String(200,"virtualization tool not selected")
		}
	})*/

	// Port running on
	r.Run(":8088")
}
