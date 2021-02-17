package docker

import (
	//log"
	"io/ioutil"
	"io"
	"context"
	"bytes"
	"os"
	"archive/tar"
	"log"
	"fmt"

	"github.com/docker/docker/api/types"
	"github.com/docker/docker/client"

	natting "github.com/docker/go-connections/nat"
	"github.com/docker/docker/api/types/container"
	network "github.com/docker/docker/api/types/network"
	"github.com/docker/docker/api/types"
)


type SSHreturn struct {
	Port     string `bson:port`
	Username string `bson:username`
    Password string `bson:password`
}

// Runs a docker contianer with default settings 
// TODO implement with public keys 
func RunVM()(interface{},error) {
	client, err := client.NewEnvClient()
	if err != nil {
		//log.Fatalf("Unable to create docker client: %s", err)
		return nil,err
	}

	// Client, imagename and Dockerfile location
	tags := []string{"p2p-ubuntu"}
	dockerfile := "./server/docker/containers/docker-ubuntu-sshd/Dockerfile"
	err = BuildImage(client, tags, dockerfile)
	if err != nil {
		//log.Println(err)
		return nil,err
	}

	// TODO Run contianer 

	imagename := "ubuntu"
	containername := "test1"
	portopening := "1003"
	inputEnv := []string{fmt.Sprintf("LISTENINGPORT=%s", portopening)}
	err = RunContainer(client, imagename, containername, portopening, inputEnv)
	if err != nil {
		return nil,err
	}

	sshreturn := new(SSHreturn)
	
	sshreturn.Port = "1003"
	sshreturn.Password = "password"
	sshreturn.Username = "master"
	
	return sshreturn, nil

}

// Taken from (https://medium.com/@Frikkylikeme/controlling-docker-with-golang-code-b213d9699998)
// Builds local docker image 
func BuildImage(client *client.Client, tags []string, dockerfile string) error{
	ctx := context.Background()

	// Create a buffer 
	buf := new(bytes.Buffer)
	tw := tar.NewWriter(buf)
	defer tw.Close()

	// Create a filereader
	dockerFileReader, err := os.Open(dockerfile)
	if err != nil {
		return err
	}

	// Read the actual Dockerfile 
	readDockerFile, err := ioutil.ReadAll(dockerFileReader)
	if err != nil {
		return err
	}

	// Make a TAR header for the file
	tarHeader := &tar.Header{
		Name: dockerfile,
		Size: int64(len(readDockerFile)),
	}

	// Writes the header described for the TAR file
	err = tw.WriteHeader(tarHeader)
    if err != nil {
		return err
    }

	// Writes the dockerfile data to the TAR file
    _, err = tw.Write(readDockerFile)
    if err != nil {
		return err
    }

    dockerFileTarReader := bytes.NewReader(buf.Bytes())

	// Define the build options to use for the file
	// https://godoc.org/github.com/docker/docker/api/types#ImageBuildOptions
	buildOptions := types.ImageBuildOptions{
        Context:    dockerFileTarReader,
        Dockerfile: dockerfile,
        Remove:     true,
		Tags: 		tags,
	}

	// Build the actual image
	imageBuildResponse, err := client.ImageBuild(
        ctx,
        dockerFileTarReader,
		buildOptions, 
	)	

	if err != nil {
		return err
	}

	// Read the STDOUT from the build process
	defer imageBuildResponse.Body.Close()
	_, err = io.Copy(os.Stdout, imageBuildResponse.Body)
	if err != nil {
		return err
	}

	return nil
}


// Taken from (https://medium.com/@Frikkylikeme/controlling-docker-with-golang-code-b213d9699998)
// Runs Docker image 
func RunContainer(client *client.Client, imagename string, containername string, port string, inputEnv []string) error {
	// Define a PORT opening
	newport, err := natting.NewPort("tcp", port)
	if err != nil {
		fmt.Println("Unable to create docker port")
		return err
	}

	// Configured hostConfig: 
	// https://godoc.org/github.com/docker/docker/api/types/container#HostConfig
	hostConfig := &container.HostConfig{
		PortBindings: natting.PortMap{
			newport: []natting.PortBinding{
				{
					HostIP:   "0.0.0.0",
					HostPort: port,
				},
			},
		},
		RestartPolicy: container.RestartPolicy{
			Name: "always",
		},
		LogConfig: container.LogConfig{
			Type:   "json-file",
			Config: map[string]string{},
		},
	}

	// Define Network config (why isn't PORT in here...?:
	// https://godoc.org/github.com/docker/docker/api/types/network#NetworkingConfig
	networkConfig := &network.NetworkingConfig{
		EndpointsConfig: map[string]*network.EndpointSettings{},
	}
	gatewayConfig := &network.EndpointSettings{
		Gateway: "gatewayname",
	}
	networkConfig.EndpointsConfig["bridge"] = gatewayConfig

	// Define ports to be exposed (has to be same as hostconfig.portbindings.newport)
	exposedPorts := map[natting.Port]struct{}{
		newport: struct{}{},
	}

	// Configuration 
	// https://godoc.org/github.com/docker/docker/api/types/container#Config
	config := &container.Config{
		Image:        imagename,
		Env: 		  inputEnv,
		ExposedPorts: exposedPorts,
		Hostname:     fmt.Sprintf("%s-hostnameexample", imagename),
	}

	// Creating the actual container. This is "nil,nil,nil" in every example.
	cont, err := client.ContainerCreate(
		context.Background(),
		config,
		hostConfig,
		networkConfig,
		containername,
	)

	if err != nil {
		log.Println(err)
		return err
	}

	// Run the actual container 
	client.ContainerStart(context.Background(), cont.ID, types.ContainerStartOptions{})
	log.Printf("Container %s is created", cont.ID)


	return nil
}
