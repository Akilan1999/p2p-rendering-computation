package server

import (
	"github.com/Akilan1999/p2p-rendering-computation/client/clientIPTable"
	"github.com/go-co-op/gocron/v2"
	"time"
)

func CRON() {
	s, _ := gocron.NewScheduler()

	s.NewJob(
		gocron.DurationJob(
			10*time.Second,
		),
		gocron.NewTask(
			func() {
				clientIPTable.RemoveOfflineNodes()
			},
		),
	)

	s.Start()

}
