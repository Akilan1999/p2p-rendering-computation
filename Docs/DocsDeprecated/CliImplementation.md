# Cli module

The Cli (i.e Command Line Interface) is the only one in which the user can directly interact with the
modules in the project. The objective when building the Cli was to have the least amount of
commands as possible. The cli was built using the library called urfave cli v2 . They were 2
major files created named as flags.go and actions.go.
### Flags.go
The flags .go file is responsible to create the appropriate flags for the cli. There are 2 types of flags
called boolean and string. Each of the flags outputs are assigned to a
variable to be handled. The flags can also detect environment variables set. This feature is useful
because if the user wants to call certain flags in a repeated sequence it only has to be initialized
once.

### Actions.go
The actions.go file is implemented to call the appropriate functions when the flags are called. It
interacts directly with the modules in the project. Action.go checks if variables
are not empty string or the boolean value is true. 
