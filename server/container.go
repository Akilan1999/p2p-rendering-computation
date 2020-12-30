package main

import (
	"context"
	"fmt"
	"io"
	"os"

	"github.com/docker/docker/api/types"
	"github.com/docker/docker/api/types/container"
	"github.com/docker/docker/client"
	// "reflect" (use to check variable type: command [fmt.Println(reflect.TypeOf(<variable>))])
)


/* creates continer 
TODO: Function inputs , outputs to be modified
*/
func create_container() (error, string){
	ctx := context.Background()
	cli, err := client.NewClientWithOpts(client.FromEnv, client.WithAPIVersionNegotiation())
	if err != nil {
		return err, ""
	}

	imageName := "bfirsh/reticulate-splines"

	out, err := cli.ImagePull(ctx, imageName, types.ImagePullOptions{})
	if err != nil {
		return err, ""
	}
	io.Copy(os.Stdout, out)

	resp, err := cli.ContainerCreate(ctx, &container.Config{
		Image: imageName,
	}, nil, nil, nil, "")

	if err != nil {
		return err, ""
	}

	if err := cli.ContainerStart(ctx, resp.ID, types.ContainerStartOptions{}); err != nil {
		return err, ""
	}

	return nil, string(resp.ID)
}

/* returns all containers running */
func containers_running()(error, []types.Container){
	ctx := context.Background()
	cli, err := client.NewClientWithOpts(client.FromEnv, client.WithAPIVersionNegotiation())
	if err != nil {
		return err ,nil
	}

	containers, err := cli.ContainerList(ctx, types.ContainerListOptions{})
	if err != nil {
		return err, nil
	}

	return nil ,containers
}

/* function to remove container from running */
func delete_container(container_id string) (error, string){
	ctx := context.Background()
	cli, err := client.NewClientWithOpts(client.FromEnv, client.WithAPIVersionNegotiation())
	if err != nil {
		return err ,""
	}
	if err := cli.ContainerStop(ctx, container_id, nil); err != nil {
		return err ,""
	}
	return nil ,"Success"
}

func main(){
	_, containers :=  containers_running()
	fmt.Println(containers)
	_, container_id := create_container()
	_, status := delete_container(container_id)
	fmt.Println(status)
}
