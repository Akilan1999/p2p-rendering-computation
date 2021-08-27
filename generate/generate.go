// Package generate The purpose of this package is to ensure that we can extend the use-case of P2PRC.
// We will create a project directory with the template to extend the use-case of P2PRC
package generate

import (
	"git.sr.ht/~akilan1999/p2p-rendering-computation/config"
	"github.com/otiai10/copy"
	"os"
	"strings"
)

type NewProject struct {
	Name 	  string
	NewDir    string
	P2PRCPath string
	Option    *copy.Options
}

// GenerateNewProject creates a new copy of the P2PRC
// project for custom modification
func GenerateNewProject(name string) error {
	// Create new variable of type NewProject
	var newProject NewProject
	// Get path of the current directory
	curDir, err := config.GetCurrentPath()
	if err != nil {
		return err
	}
	// Folder name of the new generated project
	newProject.NewDir = curDir + name + "/"
	// Create a new folder based on name entered
	err = CreateFolder(name, curDir)
	if err != nil {
		return err
	}
	// get path of P2PRC
	P2PRCPATH, err := config.GetPathP2PRC()
	if err != nil {
		return err
	}
	// Assign P2PRC path to the newly generated project
	newProject.P2PRCPath = P2PRCPATH
	// Steps:
	// - copy all files from P2PRC
	// - remove go.mod and go.sum and create new ones

	// Files we require to skip
	var Options copy.Options

	// Skip or have the appropriate files and directories not needed
	Options.Skip = func(src string) (bool, error) {
		switch {
		case strings.HasSuffix(src, "main.go"):
			return false, nil
		case strings.HasSuffix(src, ".go"):
			return true, nil
		case strings.HasSuffix(src, "go.mod"):
			return true, nil
		case strings.HasSuffix(src, "go.sum"):
			return true, nil
		case strings.HasSuffix(src, ".idea"):
			return true, nil
		case strings.HasSuffix(src, "Makefile"):
			return true, nil
		case strings.HasSuffix(src, name):
			return true, nil
		default:
			return false, nil
		}
	}

	// Storing type option in the struct new project
	newProject.Option = &Options

	// Copies all files from P2PRC to the new project created
	err = copy.Copy(newProject.P2PRCPath, newProject.NewDir,*newProject.Option)
	if err != nil {
		return err
	}

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