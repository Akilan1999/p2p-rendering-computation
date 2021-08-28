package generate

import (
	"go/parser"
	"go/printer"
	"go/token"
	"os"
)


// GetASTGoFile Gets AST of the Go file provided
func (np *NewProject)GetASTGoFile() error{
	fset := token.NewFileSet()
	node, err := parser.ParseFile(fset, np.FileNameAST, nil, parser.ParseComments)
	if err != nil {
		return err
	}
	//Write Token information to the struct
	np.Token = fset
	// Write AST information the struct
	np.AST = node
	return nil
}

// ChangeImports Changes import of the AST
func (np *NewProject)ChangeImports(CurrentImport string,ChangedImport string) error {
	// Iterating through the loop and changing the appropriate import
	for i, spec := range np.AST.Imports {
		// If the current import is found then change it
		if spec.Path.Value == "\"" + CurrentImport + "\"" {
			np.AST.Imports[i].Path.Value = "\"" + ChangedImport + "\""
		}
	}
	return nil
}

// WriteGoAst Write changed imports back to the AST
func (np *NewProject)WriteGoAst() error {
	// write new AST to file
	f, err := os.Create(np.FileNameAST)
	if err != nil {
		return nil
	}
	defer f.Close()
	if err := printer.Fprint(f, np.Token, np.AST); err != nil {
		return err
	}
	return nil
}