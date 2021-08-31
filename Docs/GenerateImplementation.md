# Generate Module 
P2PRC is a great layer of abstraction. This means that in many cases it is not an end product but rather 
a tool that customized as an end product. An example would be writing your own billing module to monetize
the computation power available. The generate module copies the current with the appropriate git histories 
and keeps only the go files which would be useful to edit. To use the generate module the user will need 
to have a go compiler present in his computer. Due to the introduction of this module there will 2 releases:

- Regular Release (Consists of only the build binary and cli command cannot access the generate module)
- Developer Release (Consists of important Go files and the cli can access the generate module)

## How does this work ?

### [Struct information](https://github.com/Akilan1999/p2p-rendering-computation/blob/9d69aed8ce0fe5273aaff2828f7d51c3d5ac2ce4/generate/generate.go#L19)
- ###```Generate.go```: 
This file creates a local copy of P2PRC from where the CLI was called from.
This go file also does various stuff like instruction of file should be ignored when copying and 
which of should not be. Now let's understand this. Below is a sample code which does the following:

```go
	//----------------------------------------------------------------
	// Action performed:
	// - Ensuring main.go file exists
	// - Skipping all .go files apart from the ones listed above
	// - Skipping .idea/ directory
	// - Skipping Makefile file
	//----------------------------------------------------------------
	Options.Skip = func(src string) (bool, error) {
		switch {
		case strings.HasSuffix(src, "main.go"):
			return false, nil
		case strings.HasSuffix(src, ".go"):
			return true, nil
		case strings.HasSuffix(src, ".idea"):
			return true, nil
		case strings.HasSuffix(src, "Makefile"):
			return true, nil
		default:
			return false, nil
		}
	}
	
	// Doing the copy 
	err = copy.Copy("<P2PRC folder you want to copy from>", "<PATH to the directory>", Options)
```

Unfortunately currently this will have to be manually edited in the ```Generate.go``` file. When using the generate 
module the user also creates their own Go module which is the modified version of P2PRC. This means 
if the 1 modified package is using another modified package then the appropriate import have to be modified 
in the file where the import is called: 

Ex:
```go
 //Sample Project module name = Test 
 //Package names:
 //- Test/Genius
 //- Test/GeGeGenuis
 //
 // When we call the generate function with the new project with the module name = MicDrop 
 // The new package name would be:
 // - MicDrop/Genius 
 // - MicDrop/GeGeGenuis
 
 // Test/Genius code depends on the package Test/GeGeGenuis
 import (
 	"Test/GeGeGenuis"
 )

// When we create a new module with the copy of the 
// existing project we need change:
import (
 "MicDrop/GeGeGenuis"
)
```

To do this we have built functions which can modify import names in the Go file provided. 
To customize the use case of your generate module you would need to manually add your own 
imports which are supposed to be replaced and in which files they are supposed to be replaced 
in. 

```go
// 1.0 - Test/Genius.go -> GeGeGenuis module
// a is struct of type NewProject 
	a.FileNameAST = "<path to project to copy from>/Test/Genius.go"
	// Get AST information of the file
	err := a.GetASTGoFile()
	if err != nil {
		return err
	}
	// Change the appropriate Go file
	err = a.ChangeImports("Test/GeGeGenuis", "MicDrop/GeGeGenuis")
	if err != nil {
		return err
	}
	// Writes the change to the appropriate file
	err = a.WriteGoAst()
	if err != nil {
		return err
	}
```

Higher order of execution of ```Generate.go```:
1. Copy entire P2PRC project and ignores files which are not meant to be copied 
2. The folder name will be based on the new project name and the module name based on the new 
   module name provided.
3. Modifies the appropriate imports in the project as instructed in the code. 
4. Creates a commit with the new changes in the new project. 


- ### ``` modifyGenerate.go```: 
  This a really simple implementation where we replace the imports 
  in certain files as instructed from ```generate.go```. To do we create an AST (i.e Abstract Syntax tree)
  from new file we want to change the imports in. AST create a tree structure of expression. To change the 
  import we can just traverse to the appropriate expression and change the value of that expression in 
  the case of modifying imports. This approach is more simple than using templates.


