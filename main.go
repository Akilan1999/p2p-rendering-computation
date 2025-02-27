package main

import (
	"log"
	"os"
	"os/signal"
	"syscall"

	"github.com/Akilan1999/p2p-rendering-computation/cmd"
	"github.com/urfave/cli/v2"
)

// VERSION specifies the version of the platform
var VERSION = "3.0.0"
var mode string

// Varaibles if mode is client
var OS, Pull_location, Run_script string
var List_servers, Ip_table bool

// To be implemented later on
func getFireSignalsChannel() chan os.Signal {

	c := make(chan os.Signal, 1)
	signal.Notify(c,
		// https://www.gnu.org/software/libc/manual/html_node/Termination-Signals.html
		syscall.SIGTERM, // "the normal way to politely ask a program to terminate"
		syscall.SIGINT,  // Ctrl+C
		syscall.SIGQUIT, // Ctrl-\
		syscall.SIGKILL, // "always fatal", "SIGKILL and SIGSTOP may not be caught by a program"
		syscall.SIGHUP,  // "terminal is disconnected"
	)
	return c

}

func exit() {
	syscall.Kill(syscall.Getpid(), syscall.SIGTERM)
}

func main() {
	app := cli.NewApp()
	app.Name = "p2p-rendering-computation"
	app.Usage = "p2p cli application to create and access VMs in other servers"
	app.Version = VERSION
	app.Flags = cmd.AppConfigFlags
	app.Action = cmd.CliAction

	err := app.Run(os.Args)
	if err != nil {
		log.Fatal(err)
	}
}
