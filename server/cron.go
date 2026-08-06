package server

import (
	"fmt"
	"github.com/Akilan1999/p2p-rendering-computation/client/clientIPTable"
	"github.com/Akilan1999/p2p-rendering-computation/config"
	"github.com/Akilan1999/p2p-rendering-computation/p2p"
	"github.com/go-co-op/gocron/v2"
	"time"
)

func CRON() {
	s, _ := gocron.NewScheduler()

	s.NewJob(
		gocron.DurationJob(
			20*time.Second,
		),
		gocron.NewTask(
			func() {
				clientIPTable.RemoveOfflineNodes()
				RestartNATIfServerNotFound()
			},
		),
	)

	s.Start()

}

// RestartNATIfServerNotFound The aim is to attempt to restart the server
// if it's not reachable from the outside.
func RestartNATIfServerNotFound() {
	config, err := config.ConfigInit(nil, nil)
	if err != nil {
		return
	}
	_, err = p2p.SearchMachine(config.MachineName)
	if err != nil {
		fmt.Println("Server attempting to restart again since it's not pingable")
		// Increasing the wait time for the NAT traversal
		NATEscapeTime += 1
		SetupServerCurrentMachine()
		return
	}
}
