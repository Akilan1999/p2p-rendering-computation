package server

import (
	"fmt"
	"github.com/gin-gonic/gin"
	"log"
	"net/http"
	//"fmt"
)

func Server() {
	r := gin.Default()

	// Gets default information of the server
	r.GET("/server_info", func(c *gin.Context) {
		c.JSON(http.StatusOK, ServerInfo())
	})

	// Route build to do a speed test
	r.GET("/upload", func(c *gin.Context) {
		file, _ := c.FormFile("file")
		log.Println(file.Filename)

		// Upload the file to specific dst.
		// c.SaveUploadedFile(file, dst)

		c.String(http.StatusOK, fmt.Sprintf("'%s' uploaded!", file.Filename))
	})



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
