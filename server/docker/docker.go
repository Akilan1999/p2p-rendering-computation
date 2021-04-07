package docker

import (
	"bufio"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"github.com/docker/docker/api/types"
	"github.com/docker/docker/api/types/container"
	"github.com/docker/docker/client"
	"github.com/docker/docker/pkg/archive"
	"github.com/docker/go-connections/nat"
	"github.com/phayes/freeport"
	"io"
	"time"
)

type DockerVM struct {
	 SSHPort int `json:"SSHPort"`
	 SSHUsername string `json:"SSHUsername"`
	 SSHPassword string `json:"SSHPassword"`
	 VNCPort int `json:"VNCPort"`
	 VNCPassword string `json:"VNCPassword"`
	 ID string `json:"ID"`
	 TagName string `json:"TagName"`
	 ImagePath string `json:"ImagePath"`
	 Ports []int `json:"OpenPorts"`
}

type ErrorLine struct {
	Error       string      `json:"error"`
	ErrorDetail ErrorDetail `json:"errorDetail"`
}

type ErrorDetail struct {
	Message string `json:"message"`
}

var dockerRegistryUserID = ""

func BuildRunContainer(NumPorts int) (*DockerVM,error) {
	//Docker Struct Variable
	var RespDocker *DockerVM = new(DockerVM)

	// Count: 2 allocated ports for VNC and SSH + other ports
	// to open
	count := NumPorts + 2

	// Gets 2 TCP ports empty
	Ports, err := freeport.GetFreePorts(count)
	if err != nil {
		return nil,err
	}

	for i := 2; i < count; i++ {
		RespDocker.Ports = append(RespDocker.Ports, Ports[i])
	}

	// Sets Free port to Struct
	RespDocker.SSHPort = Ports[0]
	RespDocker.VNCPort = Ports[1]
	// Sets appropriate username and password to the
	// variables in the struct
	RespDocker.SSHUsername = "master"
	RespDocker.SSHPassword = "password"
	RespDocker.VNCPassword = "vncpassword"

	//Default parameters
	RespDocker.TagName = "p2p-ubuntu"
	RespDocker.ImagePath = "server/docker/containers/docker-ubuntu-sshd/"

	// Gets docker information from env variables
	cli, err := client.NewClientWithOpts(client.FromEnv, client.WithAPIVersionNegotiation())
	if err != nil {
		return nil,err
	}

	// Builds docker image
	err = RespDocker.imageBuild(cli)
	if err != nil {
		return nil,err
	}

	// Runs docker contianer
	err = RespDocker.runContainer(cli)

	if err != nil {
		return nil,err
	}


	return RespDocker,nil

}

//Builds docker image (TODO: relative path for Dockerfile folder)
func (d *DockerVM)imageBuild(dockerClient *client.Client) error {
	ctx, _ := context.WithTimeout(context.Background(), time.Second*2000)
	//defer cancel()

	tar, err := archive.TarWithOptions(d.ImagePath, &archive.TarOptions{})
	if err != nil {
		return err
	}

	opts := types.ImageBuildOptions{
		Dockerfile: "Dockerfile",
		Tags:       []string{d.TagName},
		Remove:     true,
	}
	res, err := dockerClient.ImageBuild(ctx, tar, opts)
	if err != nil {
		return err
	}

	defer res.Body.Close()

	err = print(res.Body)
	if err != nil {
		return err
	}

	return nil
}

// Starts container and assigns port numbers
// Sample Docker run Command
// docker run -d=true --name=Test123 --restart=always -p 3443:6901 -p 3453:22
//-p 3434:3434 -p 3245:3245 -v=/opt/data:/data p2p-ubuntu /start > /dev/null
func (d *DockerVM)runContainer(dockerClient *client.Client) error{
	ctx, _ := context.WithTimeout(context.Background(), time.Second*2000)

	//Exposed ports for docker config file
	var ExposedPort nat.PortSet

	ExposedPort = nat.PortSet{
		"22/tcp": struct{}{},
		"6901/tcp": struct{}{},
	}

	// Port forwarding for VNC and SSH ports
	PortForwarding := nat.PortMap{
		"22/tcp": []nat.PortBinding{
			{
				HostIP: "0.0.0.0",
				HostPort: fmt.Sprint(d.SSHPort),
			},
		},
		"6901/tcp": []nat.PortBinding{
			{
				HostIP: "0.0.0.0",
				HostPort: fmt.Sprint(d.VNCPort),
			},
		},
	}

	for i := range d.Ports {

		Port, err := nat.NewPort("tcp",fmt.Sprint(d.Ports[i]))
		if err != nil {
			return err
		}

		// Exposed Ports
		ExposedPort[Port] = struct{}{}

		PortForwarding[Port] = []nat.PortBinding{
			{
				HostIP: "0.0.0.0",
				HostPort: fmt.Sprint(d.Ports[i]),
			},
		}
	}


	config := &container.Config{
		Image : d.TagName,
		Entrypoint: [] string {"/dockerstartup/vnc_startup.sh","/start"},
		Volumes: map[string]struct{}{"/opt/data:/data":{}},
		ExposedPorts: ExposedPort,
	}
	hostConfig := &container.HostConfig{
		PortBindings: PortForwarding,
	}

	res, err := dockerClient.ContainerCreate(ctx,config,hostConfig,
		nil,nil,"")

	if err != nil {
		return err
	}

	err = dockerClient.ContainerStart(ctx, res.ID, types.ContainerStartOptions{})

	if err != nil {
		return err
	}

	return nil
}


func print(rd io.Reader) error {
	var lastLine string

	scanner := bufio.NewScanner(rd)
	for scanner.Scan() {
		lastLine = scanner.Text()
	}

	errLine := &ErrorLine{}
	json.Unmarshal([]byte(lastLine), errLine)
	if errLine.Error != "" {
		return errors.New(errLine.Error)
	}

	if err := scanner.Err(); err != nil {
		return err
	}

	return nil
}