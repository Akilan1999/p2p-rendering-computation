package docker

import (
	"bufio"
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"git.sr.ht/~akilan1999/p2p-rendering-computation/config"
	"github.com/docker/docker/api/types"
	"github.com/docker/docker/api/types/container"
	"github.com/docker/docker/client"
	"github.com/docker/docker/pkg/archive"
	"github.com/docker/go-connections/nat"
	"github.com/lithammer/shortuuid"
	"github.com/phayes/freeport"
	"io"
	"os/exec"
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
	 GPU string `json:"GPU"`
}

type ErrorLine struct {
	Error       string      `json:"error"`
	ErrorDetail ErrorDetail `json:"errorDetail"`
}

type ErrorDetail struct {
	Message string `json:"message"`
}

var dockerRegistryUserID = ""

func BuildRunContainer(NumPorts int, GPU string) (*DockerVM,error) {
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

	// Sets if GPU is selected or not
	RespDocker.GPU = GPU

	// Sets Free port to Struct
	RespDocker.SSHPort = Ports[0]
	//RespDocker.VNCPort = Ports[1]
	// Sets appropriate username and password to the
	// variables in the struct
	RespDocker.SSHUsername = "master"
	RespDocker.SSHPassword = "password"
	//RespDocker.VNCPassword = "vncpassword"

	//Default parameters
	RespDocker.TagName = "p2p-ubuntu"
	// Get Path from config
	config, err := config.ConfigInit()
	if err != nil {
		return nil,err
	}
	RespDocker.ImagePath = config.DockerFile
	//if GPU == "true" {
	//	RespDocker.ImagePath = "/home/asleepyguy/p2p-rendering-computation/server/docker/containers/docker-ubuntu-sshd-gpu/"
	//}

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
// docker run -d=true --name=Test123 --restart=always --gpus all
//-p 3443:6901 -p 3453:22 -p 3434:3434 -p 3245:3245 -v=/opt/data:/data
//p2p-ubuntu /start > /dev/null
func (d *DockerVM)runContainer(dockerClient *client.Client) error{
	ctx, _ := context.WithTimeout(context.Background(), time.Second*2000)


	// The first mode runs using the Docker Api. As the API supports using
	// CPU and uses a shell script for GPU call because till this point of
	// implementation docker api does not support the flag "--gpu all"
    if d.GPU != "true" {
		//Exposed ports for docker config file
		var ExposedPort nat.PortSet

		ExposedPort = nat.PortSet{
			"22/tcp": struct{}{},
			//"6901/tcp": struct{}{},
		}

		// Port forwarding for VNC and SSH ports
		PortForwarding := nat.PortMap{
			"22/tcp": []nat.PortBinding{
				{
					HostIP: "0.0.0.0",
					HostPort: fmt.Sprint(d.SSHPort),
				},
			},
			//"6901/tcp": []nat.PortBinding{
			//	{
			//		HostIP: "0.0.0.0",
			//		HostPort: fmt.Sprint(d.VNCPort),
			//	},
			//},
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
			Image:        d.TagName,
			Entrypoint:   []string{"/start"},
			Volumes:      map[string]struct{}{"/opt/data:/data": {}},
			ExposedPorts: ExposedPort,
		}
		hostConfig := &container.HostConfig{
			PortBindings: PortForwarding,
		}

		res, err := dockerClient.ContainerCreate(ctx, config, hostConfig,
			nil, nil, "")

		// Set response ID
		d.ID = res.ID

		if err != nil {
			return err
		}

		err = dockerClient.ContainerStart(ctx, res.ID, types.ContainerStartOptions{})

		if err != nil {
			return err
		}
	} else {
		// Generate Random ID
		id := shortuuid.New()
		d.ID = id

		var cmd bytes.Buffer
		cmd.WriteString("docker run -d=true --name="+ id +" --restart=always --gpus all -p " + fmt.Sprint(d.VNCPort) + ":" + "6901 " + "-p " + fmt.Sprint(d.SSHPort) + ":" + "22 ")
		for i := range d.Ports {
			cmd.WriteString("-p " + fmt.Sprint(d.Ports[i]) + ":" + fmt.Sprint(d.Ports[i]) + " ")
		}
		cmd.WriteString("-v=/opt/data:/data p2p-ubuntu /start > /dev/null")
		//"-v=/opt/data:/data p2p-ubuntu /start > /dev/null"
		cmdStr := cmd.String()
		_, err := exec.Command("/bin/sh", "-c", cmdStr).Output()
		if err != nil {
			return err
		}
	}
	return nil
}

// StopAndRemoveContainer TODO: Implement and remove docker instance running
// Stop and remove a container
// Reference (https://gist.github.com/frikky/e2efcea6c733ea8d8d015b7fe8a91bf6)
func StopAndRemoveContainer(containername string) error {
	ctx := context.Background()

	// Gets docker information from env variables
	client, err := client.NewClientWithOpts(client.FromEnv, client.WithAPIVersionNegotiation())
	if err != nil {
		return err
	}

	if err = client.ContainerStop(ctx, containername, nil); err != nil {
		return err
	}

	removeOptions := types.ContainerRemoveOptions{
		RemoveVolumes: true,
		Force:         true,
	}

	if err = client.ContainerRemove(ctx, containername, removeOptions); err != nil {
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