package server

import (
	"github.com/gin-gonic/gin"
	"net/http"
)

func Server() {
	r := gin.Default()
	
	// Gets default information of the server
	r.GET("/server_info", func(c *gin.Context) {
		c.JSON(http.StatusOK, ServerInfo())
	})

	r.GET("/create_vm", func(c *gin.Context) {
		c.JSON(http.StatusOK, ServerInfo())
	})

	// Port running on
	r.Run(":8088")
}