package generate

import (
	"fmt"
	"testing"
)

// Tests the create folder function creates a folder
// This test will create a folder in the temporary
// directory
func TestCreateFolder(t *testing.T) {
	err := CreateFolder("test", "/tmp/")
	if err != nil {
		t.Error(err)
	}
}

// Testing if a new project is created successfully
func TestGenerateNewProject(t *testing.T) {
	// Checking if a new project is created successfully
	err := GenerateNewProject("p2prctest")
	if err != nil {
		fmt.Println(err)
		t.Error(err)
	}
}