package docker

import (
	"fmt"
	"testing"
)

func TestDocker(t *testing.T) {
	//TODO overwrite with custom docker paths
	resp,err := BuildRunContainer(2,"true","")

	if err != nil {
		t.Error(err)
	}

	fmt.Print(resp.VNCPort)
}

func TestViewAllContainers(t *testing.T) {
	_,err := ViewAllContainers()

	if err != nil {
		t.Error(err)
	}

}
