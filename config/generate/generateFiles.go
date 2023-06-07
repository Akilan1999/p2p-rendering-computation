package generate

import (
	"github.com/Akilan1999/p2p-rendering-computation/p2p"
	"github.com/go-git/go-git/v5"
	"os"
)

// GenerateFiles Generates all the files needed to setup P2PRC
func GenerateFiles(rootNodes ...p2p.IpAddress) (err error) {
	err = GenerateIPTableFile(rootNodes)
	err = GenerateDockerFiles()
	err = GeneratePluginDirectory()
	err = GenerateClientTrackContainers()
	return
}

// GenerateIPTableFile Generates the IPTable file with the appropirate root node
func GenerateIPTableFile(rootNodes []p2p.IpAddress) (err error) {
	var rootnodes p2p.IpAddresses
	var rootnode p2p.IpAddress

	err = CreateIPTableFolderStructure()
	if err != nil {
		return err
	}

	// If root node addresses are not provided as optional parameters
	if len(rootNodes) <= 0 {
		rootnode.Name = "Node1"
		rootnode.ServerPort = "8088"
		rootnode.NAT = "False"
		rootnode.Ipv4 = "64.227.168.102"

		rootnodes.IpAddress = append(rootnodes.IpAddress, rootnode)
	} else {
		// if root nodes are provided then override them as the optional parameter
		for i := range rootNodes {
			rootnodes.IpAddress = append(rootnodes.IpAddress, rootNodes[i])
		}
	}

	err = rootnodes.WriteIpTable()
	return
}

// CreateIPTableFolderStructure Create folder structure for IPTable
func CreateIPTableFolderStructure() (err error) {
	path, err := GetCurrentPath()
	if err != nil {
		return err
	}
	if _, err = os.Stat(path + "p2p"); os.IsNotExist(err) {
		if err = os.Mkdir(path+"p2p", os.ModePerm); err != nil {
			return err
		}
	}
	if _, err = os.Stat(path + "p2p/iptable"); os.IsNotExist(err) {
		if err = os.Mkdir(path+"p2p/iptable", os.ModePerm); err != nil {
			return err
		}
	}
	if _, err = os.Stat(path + "p2p/iptable/ip_table.json"); os.IsNotExist(err) {
		_, err = os.Create(path + "p2p/iptable/ip_table.json")
		if err != nil {
			return err
		}
	}

	return
}

// GenerateDockerFiles Generate default docker files
func GenerateDockerFiles() (err error) {
	path, err := GetCurrentPath()
	if err != nil {
		return err
	}
	if _, err = os.Stat(path + "server"); os.IsNotExist(err) {
		if err = os.Mkdir(path+"server", os.ModePerm); err != nil {
			return err
		}
		if _, err = os.Stat(path + "server/docker"); os.IsNotExist(err) {
			if err = os.Mkdir(path+"server/docker", os.ModePerm); err != nil {
				return err
			}
		}
		if _, err = os.Stat(path + "server/docker/containers"); os.IsNotExist(err) {
			if err = os.Mkdir(path+"server/docker/containers", os.ModePerm); err != nil {
				return err
			}
		}
		if _, err = os.Stat(path + "server/docker/containers"); os.IsNotExist(err) {
			if err = os.Mkdir(path+"server/docker/containers", os.ModePerm); err != nil {
				return err
			}
		}
		if _, err = os.Stat(path + "server/docker/containers/docker-ubuntu-sshd"); os.IsNotExist(err) {
			if err = os.Mkdir(path+"server/docker/containers/docker-ubuntu-sshd", os.ModePerm); err != nil {
				return err
			}
		}

	}

	// Clone base docker image
	_, err = git.PlainClone(path+"server/docker/containers/docker-ubuntu-sshd", false, &git.CloneOptions{
		URL:      "https://github.com/Akilan1999/docker-ubuntu-sshd",
		Progress: os.Stdout,
	})
	if err != nil {
		return err
	}

	return
}

// GeneratePluginDirectory Generates plugin directory structure
func GeneratePluginDirectory() (err error) {
	path, err := GetCurrentPath()
	if err != nil {
		return err
	}
	if _, err = os.Stat(path + "plugin"); os.IsNotExist(err) {
		if err = os.Mkdir(path+"plugin", os.ModePerm); err != nil {
			return err
		}
		if _, err = os.Stat(path + "plugin/deploy"); os.IsNotExist(err) {
			if err = os.Mkdir(path+"plugin/deploy", os.ModePerm); err != nil {
				return err
			}
		}
	}
	return
}

func GenerateClientTrackContainers() (err error) {
	path, err := GetCurrentPath()
	if err != nil {
		return err
	}
	if _, err = os.Stat(path + "client"); os.IsNotExist(err) {
		if err = os.Mkdir(path+"client", os.ModePerm); err != nil {
			return err
		}

		if _, err = os.Stat(path + "client/trackcontainers.json"); os.IsNotExist(err) {
			_, err = os.Create(path + "client/trackcontainers.json")
			if err != nil {
				return err
			}
		}

		if _, err = os.Stat(path + "client/grouptrackcontainers.json"); os.IsNotExist(err) {
			_, err = os.Create(path + "client/grouptrackcontainers.json")
			if err != nil {
				return err
			}
		}

	}
	return
}
