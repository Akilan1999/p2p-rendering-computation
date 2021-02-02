package server

import (
	"github.com/shirou/gopsutil/cpu"
    "github.com/shirou/gopsutil/disk"
    "github.com/shirou/gopsutil/host"
	"github.com/shirou/gopsutil/mem"
)

// SysInfo saves the basic system information
type SysInfo struct {
    Hostname string `bson:hostname`
    Platform string `bson:platform`
    CPU      string `bson:cpu`
    RAM      uint64 `bson:ram`
    Disk     uint64 `bson:disk`
}

func ServerInfo() interface{}{
    hostStat, _ := host.Info()
    cpuStat, _ := cpu.Info()
	vmStat, _ := mem.VirtualMemory()

	info := new(SysInfo)
	
	filesystem := "/"
	
	// If the server is running windows
	if info.Hostname == "windows"{
		filesystem = "\\"
	}

    diskStat, _ := disk.Usage(filesystem) // If you're in Unix change this "\\" for "/"

    info.Hostname = hostStat.Hostname
    info.Platform = hostStat.Platform
    info.CPU = cpuStat[0].ModelName
    info.RAM = vmStat.Total / 1024 / 1024
    info.Disk = diskStat.Total / 1024 / 1024

    return info

}

