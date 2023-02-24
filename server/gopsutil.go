package server

import (
	"github.com/shirou/gopsutil/v3/cpu"
	"github.com/shirou/gopsutil/v3/disk"
	"github.com/shirou/gopsutil/v3/host"
	"github.com/shirou/gopsutil/v3/mem"
)

// SysInfo saves the basic system information
type SysInfo struct {
	Hostname string `bson:hostname`
	Platform string `bson:platform`
	CPU      string `bson:cpu`
	RAM      uint64 `bson:ram`
	Disk     uint64 `bson:disk`
	GPU      *Query `xml: GpuInfo`
}

func ServerInfo() *SysInfo {
	hostStat, _ := host.Info()
	cpuStat, _ := cpu.Info()
	vmStat, _ := mem.VirtualMemory()

	info := new(SysInfo)

	filesystem := "/"

	// If the server is running windows
	if info.Hostname == "windows" {
		filesystem = "\\"
	}

	diskStat, _ := disk.Usage(filesystem) // If you're in Unix change this "\\" for "/"

	info.Hostname = hostStat.Hostname
	info.Platform = hostStat.Platform
	info.CPU = cpuStat[0].ModelName
	info.RAM = vmStat.Total / 1024 / 1024
	info.Disk = diskStat.Total / 1024 / 1024

	gpu, err := GPUInfo()

	if err == nil {
		info.GPU = gpu
	}

	return info

}
