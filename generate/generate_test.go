package generate

import (
	"fmt"
	"git.sr.ht/~akilan1999/p2p-rendering-computation/config"
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
	err := GenerateNewProject("p2prctest","p2prctest")
	if err != nil {
		fmt.Println(err)
		t.Error(err)
	}
}

// Testing AST function to ensure imports are
// working as intended
func TestChangingImportAST(t *testing.T) {
   // Create a new variable of type NewProject
	var np NewProject
	// Get current directory
	path, err := config.GetCurrentPath()
	if err != nil {
		fmt.Println(err)
		t.Error(err)
	}
	// Create testcase scenario
	err = config.Copy(path + "testcaseAST.go", path + "/Test/testcaseAST.go")
	if err != nil {
		fmt.Println(err)
		t.Error(err)
	}
	// Sets new directory to the folder test
	np.NewDir = path + "Test/"
	// Sets file name to be opened and modified in the AST
	np.FileNameAST = path + "Test/" + "testcaseAST.go"
	// Call the Read AST function
	err = np.GetASTGoFile()
	if err != nil {
		fmt.Println(err)
		t.Error(err)
	}
	// Change an import
	err = np.ChangeImports("fmt", "lolol")
	if err != nil {
		fmt.Println(err)
		t.Error(err)
	}
	// Write those saved changes
	err = np.WriteGoAst()
	if err != nil {
		fmt.Println(err)
		t.Error(err)
	}

}

// Testing the if Go Mod is created
func TestNewProject_CreateGoMod(t *testing.T) {
	// Create a new variable of type NewProject
	var np NewProject
	path, err := config.GetCurrentPath()
	if err != nil {
		fmt.Println(err)
		t.Error(err)
	}
	// Set new project name as Test
	np.Name = "Test"
	// Set new project module as github.com/Test
	np.Module = "github.com/Test"
	// Set Path of the new project
	np.NewDir = path + "Test/"
	// Creating a go.mod file
	err = np.CreateGoMod()
	if err != nil {
		fmt.Println(err)
		t.Error(err)
	}
}

// Testing if the current go module is returned
func TestNewProject_GetCurrentGoModule(t *testing.T) {
	// Create a new variable of type NewProject
	var np NewProject
	path, err := config.GetPathP2PRC()
	if err != nil {
		fmt.Println(err)
		t.Error(err)
	}
	// Set Current project path
	np.P2PRCPath = path
	// Get module name
	err = np.GetCurrentGoModule()
	if err != nil {
		fmt.Println(err)
		t.Error(err)
	}
	fmt.Println(np.CurrentModule)
}