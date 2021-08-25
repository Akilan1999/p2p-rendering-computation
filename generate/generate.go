// Package generate The purpose of this package is to ensure that we can extend the use-case of P2PRC.
// We will create a project directory with the template to extend the use-case of P2PRC
package generate

import (
	"fmt"
	"github.com/otiai10/copy"
	"os"
	"strings"
)

func GenerateNewProject(name string) error {
	// Get path of the current directory
	curDir := os.Getenv("PWD")
	// Add slash to the end
	curDir = curDir + "/"
	// Folder name of the new generated project
	NewProject := curDir + name + "/"
	fmt.Println(NewProject)
	// Create a new folder based on name entered
	err := CreateFolder(name, curDir)
	if err != nil {
		return err
	}
	// get path of P2PRC
	P2PRCPATH := os.Getenv("P2PRC")
	// Add slash to the end
	P2PRCPATH = P2PRCPATH + "/"
	// Steps:
	// - copy all files from P2PRC
	// - remove go.mod and go.sum and create new ones

	// Files we require to skip
	var Options copy.Options

	// Skips the appropriate files and directories not needed
	Options.Skip = func(src string) (bool, error) {
		switch {
		case strings.HasSuffix(src, "go.mod"):
			return true, nil
		case strings.HasSuffix(src, "go.sum"):
			return true, nil
		case strings.HasSuffix(src, name):
			return true, nil
		default:
			return false, nil
		}
	}

	// Copies all files from P2PRC to the new project created
	err = copy.Copy(P2PRCPATH, NewProject,Options)
	if err != nil {
		return err
	}
	// Installing new project
	//cmd := exec.Command("sh","install.sh",name)
	//if err := cmd.Run(); err != nil {
	//	return err
	//}

	return nil
}

// CreateFolder Creates a new folder based on the name and path provided
func CreateFolder(name string,path string) error {
	//Create a folder/directory at a full qualified path
	err := os.Mkdir(path + name, 0755)
	if err != nil {
		return err
	}
	return nil
}