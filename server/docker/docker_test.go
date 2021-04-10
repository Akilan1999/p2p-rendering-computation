package docker

import (
	"fmt"
	"testing"
)

func TestDocker(t *testing.T) {
	resp,err := BuildRunContainer(2,"false")

	if err != nil {
		t.Error(err)
	}

	fmt.Print(resp.VNCPort)
}
