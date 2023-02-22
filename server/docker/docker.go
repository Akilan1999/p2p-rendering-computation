package docker

import (
	"bufio"
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"github.com/Akilan1999/p2p-rendering-computation/config"
	"github.com/docker/docker/api/types"
	"github.com/docker/docker/api/types/container"
	"github.com/docker/docker/client"
	"github.com/docker/docker/pkg/archive"
	"github.com/docker/go-connections/nat"
	"github.com/lithammer/shortuuid"
	"github.com/phayes/freeport"
	"io"
	"io/ioutil"
	"os/exec"
	"time"
)

type DockerVM struct {
	SSHUsername string `json:"SSHUsername"`
	SSHPassword string `json:"SSHPassword"`
	ID          string `json:"ID"`
	TagName     string `json:"TagName"`
	ImagePath   string `json:"ImagePath"`
	Ports       Ports  `json:"Ports"`
	GPU         string `json:"GPU"`
}

type DockerContainers struct {
	DockerContainer []DockerContainer `json:"DockerContainer"`
}

type DockerContainer struct {
	ContainerName        string `json:"DockerContainerName"`
	ContainerDescription string `json:"ContainerDescription"`
}

type Ports struct {
	PortSet []Port `json:"Port"`
}
type Port struct {
	PortName     string `json:"PortName"`
	InternalPort int    `json:"InternalPort"`
	Type         string `json:"Type"`
	ExternalPort int    `json:"ExternalPort"`
	IsUsed       bool   `json:"IsUsed"`
	Description  string `json:"Description"`
}

type ErrorLine struct {
	Error       string      `json:"error"`
	ErrorDetail ErrorDetail `json:"errorDetail"`
}

type ErrorDetail struct {
	Message string `json:"message"`
}

var dockerRegistryUserID = ""

// BuildRunContainer Function is incharge to invoke building and running contianer and also allocating external
// ports
func BuildRunContainer(NumPorts int, GPU string, ContainerName string) (*DockerVM, error) {
	//Docker Struct Variable
	var RespDocker *DockerVM = new(DockerVM)

	// Sets if GPU is selected or not
	RespDocker.GPU = GPU

	// Sets Free port to Struct
	//RespDocker.SSHPort = Ports[0]
	//RespDocker.VNCPort = Ports[1]
	// Sets appropriate username and password to the
	// variables in the struct
	RespDocker.SSHUsername = "master"
	RespDocker.SSHPassword = "password"
	//RespDocker.VNCPassword = "vncpassword"

	//Default parameters
	RespDocker.TagName = "p2p-ubuntu"
	// Get Path from config
	config, err := config.ConfigInit(nil)
	if err != nil {
		return nil, err
	}
	RespDocker.ImagePath = config.DefaultDockerFile

	// We are checking if the container name is not nil and not equal to the default one used
	// which is docker-ubuntu-sshd
	if ContainerName != "" && ContainerName != "docker-ubuntu-sshd" {
		Containers, err := ViewAllContainers()
		if err != nil {
			return nil, err
		}

		for _, dockerContainer := range Containers.DockerContainer {
			if dockerContainer.ContainerName == ContainerName {
				RespDocker.ImagePath = config.DockerContainers + ContainerName + "/"
				RespDocker.TagName = ContainerName
				break
			}
		}
		if RespDocker.ImagePath == config.DefaultDockerFile {
			return nil, errors.New("Container " + ContainerName + " does not exist in the server")
		}
	}

	PortsInformation, err := OpenPortsFile(RespDocker.ImagePath + "/ports.json")
	if err != nil {
		return nil, err
	}

	// Number of perts we want to open + number of ports required inside the
	// docker container
	count := NumPorts + len(PortsInformation.PortSet)
	// Creates number of ports
	OpenPorts, err := freeport.GetFreePorts(count)
	if err != nil {
		return nil, err
	}
	// Allocate external ports to ports available in the ports.json file
	for i := range PortsInformation.PortSet {
		// Setting external ports
		PortsInformation.PortSet[i].ExternalPort = OpenPorts[i]
		PortsInformation.PortSet[i].IsUsed = true
	}
	//Length of Ports allocated from thr port file
	portFileLength := len(PortsInformation.PortSet)
	// Allocate New ports the user wants to generate
	for i := 0; i < NumPorts; i++ {
		var TempPort Port
		TempPort.PortName = "AutoGen Port"
		TempPort.Type = "tcp"
		TempPort.InternalPort = OpenPorts[portFileLength+i]
		TempPort.ExternalPort = OpenPorts[portFileLength+i]
		TempPort.Description = "Auto generated TCP port"
		TempPort.IsUsed = false
		//Append temp port to port information
		PortsInformation.PortSet = append(PortsInformation.PortSet, TempPort)
	}
	// Setting ports to the docker VM struct
	RespDocker.Ports = *PortsInformation

	// Gets docker information from env variables
	cli, err := client.NewClientWithOpts(client.FromEnv, client.WithAPIVersionNegotiation())
	if err != nil {
		return nil, err
	}

	// Builds docker image
	err = RespDocker.imageBuild(cli)
	if err != nil {
		return nil, err
	}

	// Runs docker contianer
	err = RespDocker.runContainer(cli)

	if err != nil {
		return nil, err
	}

	return RespDocker, nil

}

// Builds docker image (TODO: relative path for Dockerfile deploy)
func (d *DockerVM) imageBuild(dockerClient *client.Client) error {
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
// -p 3443:6901 -p 3453:22 -p 3434:3434 -p 3245:3245 -v=/opt/data:/data
// p2p-ubuntu /start > /dev/null
func (d *DockerVM) runContainer(dockerClient *client.Client) error {
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
			//"22/tcp": []nat.PortBinding{
			//	{
			//		HostIP: "0.0.0.0",
			//		HostPort: fmt.Sprint(d.SSHPort),
			//	},
			//},
			//"6901/tcp": []nat.PortBinding{
			//	{
			//		HostIP: "0.0.0.0",
			//		HostPort: fmt.Sprint(d.VNCPort),
			//	},
			//},
		}

		for i := range d.Ports.PortSet {
			// Parameters "tcp or udp", external port
			Port, err := nat.NewPort(d.Ports.PortSet[i].Type, fmt.Sprint(d.Ports.PortSet[i].InternalPort))
			if err != nil {
				return err
			}

			// Exposed Ports
			ExposedPort[Port] = struct{}{}

			PortForwarding[Port] = []nat.PortBinding{
				{
					HostIP:   "0.0.0.0",
					HostPort: fmt.Sprint(d.Ports.PortSet[i].ExternalPort),
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
		cmd.WriteString("docker run -d=true --name=" + id + " --restart=always --gpus all ")
		for i := range d.Ports.PortSet {
			cmd.WriteString("-p " + fmt.Sprint(d.Ports.PortSet[i].ExternalPort) + ":" + fmt.Sprint(d.Ports.PortSet[i].InternalPort) + " ")
		}
		cmd.WriteString("-v=/opt/data:/data " + d.TagName + " /start > /dev/null")
		//"-v=/opt/data:/data p2p-ubuntu /start > /dev/null"
		cmdStr := cmd.String()
		_, err := exec.Command("/bin/sh", "-c", cmdStr).Output()
		if err != nil {
			return err
		}
	}
	return nil
}

// StopAndRemoveContainer
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

// ViewAllContainers returns all containers runnable and which can be built
func ViewAllContainers() (*DockerContainers, error) {
	// Traverse the deploy path as per given in the config file
	config, err := config.ConfigInit(nil)
	if err != nil {
		return nil, err
	}

	folders, err := ioutil.ReadDir(config.DockerContainers)
	if err != nil {
		return nil, err
	}

	//Declare variable DockerContainers of type struct
	var Containers *DockerContainers = new(DockerContainers)

	for _, f := range folders {
		if f.IsDir() {
			//Declare variable DockerContainer of type struct
			var Container DockerContainer

			// Setting container name to deploy name
			Container.ContainerName = f.Name()
			// Getting Description from file description.txt
			Description, err := ioutil.ReadFile(config.DockerContainers + "/" + Container.ContainerName + "/description.txt")
			// if we os.Open returns an error then handle it
			if err != nil {
				return nil, err
			}

			// Get Description from description.txt
			Container.ContainerDescription = string(Description)

			Containers.DockerContainer = append(Containers.DockerContainer, Container)
		}
	}

	return Containers, nil
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

func OpenPortsFile(filename string) (*Ports, error) {
	buf, err := ioutil.ReadFile(filename)
	if err != nil {
		return nil, err
	}

	c := &Ports{}
	err = json.Unmarshal(buf, c)
	if err != nil {
		return nil, fmt.Errorf("in file %q: %v", filename, err)
	}

	return c, nil
}
