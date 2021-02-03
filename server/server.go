package server

import (
	"github.com/gin-gonic/gin"
	"net/http"
	docker "git.sr.ht/~akilan1999/p2p-rendering-computation/server/docker"
)

func Server() {
	r := gin.Default()
	
	// Gets default information of the server
	r.GET("/server_info", func(c *gin.Context) {
		c.JSON(http.StatusOK, ServerInfo())
	})

	r.GET("/create_vm", func(c *gin.Context) {
		c.JSON(http.StatusOK, docker.BuildContainer())
	})

	// Port running on
	r.Run(":8088")
}