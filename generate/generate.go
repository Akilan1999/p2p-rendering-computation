// Package generate The purpose of this package is to ensure that we can extend the use-case of P2PRC.
// We will create a project directory with the template to extend the use-case of P2PRC
package generate

import (
	"fmt"
	"git.sr.ht/~akilan1999/p2p-rendering-computation/config"
	"github.com/otiai10/copy"
	"go/ast"
	"go/token"
	modfile "golang.org/x/mod/modfile"
	"io/ioutil"
	"os"
	"os/exec"
	"strings"
)

// NewProject Struct information required when creating a new project
type NewProject struct {
	Name		string
	Module		string
	NewDir		string
	P2PRCPath	string
	CurrentModule	string
	Option		*copy.Options
	Token		*token.FileSet
	AST		*ast.File
	FileNameAST	string
}

// GenerateNewProject creates a new copy of the P2PRC
// project for custom modification
func GenerateNewProject(name string, module string) error {
	// Create new variable of type NewProject
	var newProject NewProject
	//Setting module name to the new project
	newProject.Module = module
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

	// IF YOU ARE RELEASING AN EXTENSION OF P2PRC
	// MODIFY HERE (AN EXTENSION FROM YOUR EXTENSION :P)
	// Skip or have the appropriate files and directories not needed
	//----------------------------------------------------------------
	// Action performed:
	// - Ensuring main.go file exists
	// - Ensuring generate.go file exists
	// - Ensuring modifyGenerate.go file exists
	// - Ensuring generate_test.go file exists
	// - Ensuring server/server.go file exists
	// - Ensuring server/gopsutil.go file exists
	// - Ensuring server/gpu.go file exists
	// - Ensuring cmd/action.go file exists
	// - Ensuring cmd/flags.go file exists
	// - Skipping all .go files apart from the ones listed above
	// - Skipping go.mod file
	// - Skipping go.sum file
	// - Skipping .idea/ directory
	// - Skipping Makefile file
	// - Skipping <Project Name>/ directory
	//----------------------------------------------------------------
	Options.Skip = func(src string) (bool, error) {
		switch {
		case strings.HasSuffix(src, "main.go"):
			return false, nil
		case strings.HasSuffix(src, "generate.go"):
			return false, nil
		case strings.HasSuffix(src, "modifyGenerate.go"):
			return false, nil
		case strings.HasSuffix(src, "generate_test.go"):
			return false, nil
		case strings.HasSuffix(src, "server/server.go"):
			return false, nil
		case strings.HasSuffix(src, "server/gopsutil.go"):
			return false, nil
		case strings.HasSuffix(src, "server/gpu.go"):
			return false, nil
		case strings.HasSuffix(src, "cmd/action.go"):
			return false, nil
		case strings.HasSuffix(src, "cmd/flags.go"):
			return false, nil
		case strings.HasSuffix(src, "config/config.go"):
			return false, nil
		case strings.HasSuffix(src, "config/config_test.go"):
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
	err = copy.Copy(newProject.P2PRCPath, newProject.NewDir, *newProject.Option)
	if err != nil {
		return err
	}

	// Creating a new go.mod file in the appropriate directory
	err = newProject.CreateGoMod()
	if err != nil {
		return err
	}

	// Get current project mod name
	err = newProject.GetCurrentGoModule()
	if err != nil {
		return err
	}

	// Change the appropriate imports
	err = newProject.ChangeImportFiles()
	if err != nil {
		return err
	}

	// Add changes inside the new project
	err = newProject.GitAdd()
	if err != nil {
		return err
	}

	// commit changes inside the new project
	err = newProject.GitCommit()
	if err != nil {
		return err
	}
	// Creates go.sum file
	//err = newProject.CreateGoModTidy()
	//if err != nil {
	//	return err
	//}

	return nil
}

// CreateFolder Creates a new folder based on the name and path provided
func CreateFolder(name string, path string) error {
	//Create a folder/directory at a full qualified path
	err := os.Mkdir(path+name, 0755)
	if err != nil {
		return err
	}
	return nil
}

// CreateGoMod Creates a new go module for the new project created
func (a *NewProject) CreateGoMod() error {
	// Create new go.mod in the appropriate directory
	cmd := exec.Command("go", "mod", "init", a.Module)
	cmd.Dir = a.NewDir
	if err := cmd.Run(); err != nil {
		return err
	}
	return nil
}

func (a *NewProject) CreateGoModTidy() error {
	// Installs all the appropriate dependencies and creates go.sum
	cmd := exec.Command("go", "mod", "tidy")
	cmd.Dir = a.NewDir
	if err := cmd.Run(); err != nil {
		return err
	}
	return nil
}

// ChangeImportFiles Changes Appropriate imports in the appropriate file
func (a *NewProject) ChangeImportFiles() error {
	// IF YOU ARE RELEASING AN EXTENSION OF P2PRC
	// MODIFY HERE (AN EXTENSION FROM YOUR EXTENSION :P)
	//----------------------------------------------------------------
	// Action performed:
	// Files we would need to modify the imports in
	// - generate/generate.go -> config module
	// - generate/generate_test.go -> config module
	// - cmd/action.go -> config module, server module, generate module
	// - cmd/flags.go -> config module, server module, generate module
	// - server/server.go -> config module
	// - main.go -> cmd module
	//-----------------------------------------------------------------

	//-----------------------------------------------------------------
	// 1.0 - generate/generate.go -> config module
	a.FileNameAST = a.NewDir + "generate/generate.go"
	// Get AST information of the file
	err := a.GetASTGoFile()
	if err != nil {
		return err
	}
	// Change the appropriate Go file
	err = a.ChangeImports(a.CurrentModule+"/config", a.Module+"/config")
	if err != nil {
		return err
	}
	// Writes the change to the appropriate file
	err = a.WriteGoAst()
	if err != nil {
		return err
	}
	//-----------------------------------------------------------------
	// 1.1 - generate/generate_test.go -> config module
	a.FileNameAST = a.NewDir + "generate/generate_test.go"
	// Get AST information of the file
	err = a.GetASTGoFile()
	if err != nil {
		return err
	}
	// Change the appropriate Go file
	err = a.ChangeImports(a.CurrentModule+"/config", a.Module+"/config")
	if err != nil {
		return err
	}
	// Writes the change to the appropriate file
	err = a.WriteGoAst()
	if err != nil {
		return err
	}

	//-----------------------------------------------------------------
	//-----------------------------------------------------------------
	// 2.0 - cmd/action.go -> config module, server module, generate module
	a.FileNameAST = a.NewDir + "cmd/action.go"
	// Get AST information of the file
	err = a.GetASTGoFile()
	if err != nil {
		return err
	}
	// Change the appropriate Go file
	err = a.ChangeImports(a.CurrentModule+"/config", a.Module+"/config")
	if err != nil {
		return err
	}
	err = a.ChangeImports(a.CurrentModule+"/server", a.Module+"/server")
	if err != nil {
		return err
	}
	err = a.ChangeImports(a.CurrentModule+"/generate", a.Module+"/generate")
	if err != nil {
		return err
	}
	// Writes the change to the appropriate file
	err = a.WriteGoAst()
	if err != nil {
		return err
	}
	//-----------------------------------------------------------------
	// 2.1 - cmd/flags.go -> config module, server module, generate module
	a.FileNameAST = a.NewDir + "cmd/flags.go"
	// Get AST information of the file
	err = a.GetASTGoFile()
	if err != nil {
		return err
	}
	// Change the appropriate Go file
	err = a.ChangeImports(a.CurrentModule+"/config", a.Module+"/config")
	if err != nil {
		return err
	}
	err = a.ChangeImports(a.CurrentModule+"/server", a.Module+"/server")
	if err != nil {
		return err
	}
	err = a.ChangeImports(a.CurrentModule+"/generate", a.Module+"/generate")
	if err != nil {
		return err
	}
	// Writes the change to the appropriate file
	err = a.WriteGoAst()
	if err != nil {
		return err
	}
	//-----------------------------------------------------------------
	//-----------------------------------------------------------------
	// 3.0 - server/server.go -> config module
	a.FileNameAST = a.NewDir + "server/server.go"
	// Get AST information of the file
	err = a.GetASTGoFile()
	if err != nil {
		return err
	}
	// Change the appropriate Go file
	err = a.ChangeImports(a.CurrentModule+"/config", a.Module+"/config")
	if err != nil {
		return err
	}
	// Writes the change to the appropriate file
	err = a.WriteGoAst()
	if err != nil {
		return err
	}
	//-----------------------------------------------------------------
	//-----------------------------------------------------------------
	// 4.0 - server/server.go -> config module
	a.FileNameAST = a.NewDir + "main.go"
	// Get AST information of the file
	err = a.GetASTGoFile()
	if err != nil {
		return err
	}
	// Change the appropriate Go file
	err = a.ChangeImports(a.CurrentModule+"/cmd", a.Module+"/cmd")
	if err != nil {
		return err
	}
	// Writes the change to the appropriate file
	err = a.WriteGoAst()
	if err != nil {
		return err
	}
	//-----------------------------------------------------------------

	return nil
}

// GetCurrentGoModule Gets the current go module name
func (a *NewProject) GetCurrentGoModule() error {
	goModBytes, err := ioutil.ReadFile(a.P2PRCPath + "go.mod")
	if err != nil {
		return err
	}
	modName := modfile.ModulePath(goModBytes)
	fmt.Fprintf(os.Stdout, "modName=%+v\n", modName)

	// Set current module to struct of file NewProject
	a.CurrentModule = modName

	return nil
}

func (a *NewProject) GitAdd() error {
	// Installs all the appropriate dependencies and creates go.sum
	cmd := exec.Command("git", "add", ".")
	cmd.Dir = a.NewDir
	if err := cmd.Run(); err != nil {
		return err
	}
	return nil
}

func (a *NewProject) GitCommit() error {
	// Installs all the appropriate dependencies and creates go.sum
	cmd := exec.Command("git", "commit", "-m=removed appropriate go files")
	cmd.Dir = a.NewDir
	if err := cmd.Run(); err != nil {
		return err
	}
	return nil
}
