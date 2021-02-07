package server

import (
	"github.com/gin-gonic/gin"
	"net/http"
	"fmt"
	docker "git.sr.ht/~akilan1999/p2p-rendering-computation/server/docker"
)

func Server() {
	r := gin.Default()
	
	// Gets default information of the server
	r.GET("/server_info", func(c *gin.Context) {
		c.JSON(http.StatusOK, ServerInfo())
	})

	r.GET("/create_vm/:virtualization", func(c *gin.Context) {
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
	})

	// Port running on
	r.Run(":8088")
}