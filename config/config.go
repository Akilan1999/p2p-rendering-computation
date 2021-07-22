package config

import (
	"github.com/spf13/viper"
	"io"
	"os"
)

var (
	defaultPath string
	defaults = map[string]interface{}{
            "IPTable": "/etc/p2p-rendering/ip_table.json",
            "DockerContainers": "/home/akilan/Documents/p2prendering/p2p-redering-computation/server/docker/containers/",
            "DefaultDockerFile": "/home/akilan/Documents/p2prendering/p2p-redering-computation/server/docker/containers/docker-ubuntu-sshd/",
            "SpeedTestFile":"/etc/p2p-rendering/50.bin",
    }
	configName = "config"
	configType = "json"
	configFile = "config.json"
	configPaths []string
)

type Config struct {
	IPTable             string
	DockerContainers    string
	DefaultDockerFile   string
	SpeedTestFile       string
	IPV6Address         string
	PluginPath          string
	TrackContainersPath string
	ServerPort          string
	//NetworkInterface  string
	//NetworkInterfaceIPV6Index int
}

// Exists reports whether the named file or directory exists.
func fileExists(name string) bool {
	if _, err := os.Stat(name); err != nil {
		if os.IsNotExist(err) {
			return false
		}
	}
	return true
}

// Copy the src file to dst. Any existing file will be overwritten and will not
// copy file attributes.
// Source: https://stackoverflow.com/questions/21060945/simple-way-to-copy-a-file
func Copy(src, dst string) error {
	in, err := os.Open(src)
	if err != nil {
		return err
	}
	defer in.Close()

	out, err := os.Create(dst)
	if err != nil {
		return err
	}
	defer out.Close()

	_, err = io.Copy(out, in)
	if err != nil {
		return err
	}
	return out.Close()
}


// SetDefaults This function to be called only during a
// make install
func SetDefaults() error {
	//Getting Current Directory from environment variable
	curDir := os.Getenv("P2PRC")

	//Setting current directory to default path
	defaultPath = curDir + "/"

	//Creates ip_table.json in the json directory
	err := Copy("p2p/ip_table.json","p2p/iptable/ip_table.json")
	if err != nil {
		return err
	}

	//Creates a copy of trackcontainers.json in the appropriate directory
	err = Copy("client/trackcontainers.json","client/trackcontainers/trackcontainers.json")
	if err != nil {
		return err
	}


	//Setting default paths for the config file
	defaults["IPTable"] = defaultPath + "p2p/iptable/ip_table.json"
	defaults["DefaultDockerFile"] = defaultPath + "server/docker/containers/docker-ubuntu-sshd/"
	defaults["DockerContainers"] = defaultPath + "server/docker/containers/"
	defaults["SpeedTestFile"] = defaultPath + "p2p/50.bin"
	defaults["IPV6Address"] = ""
	defaults["PluginPath"] = defaultPath + "plugin/deploy"
	defaults["TrackContainersPath"] = defaultPath + "client/trackcontainers/trackcontainers.json"
	defaults["ServerPort"] = "8088"
	//defaults["NetworkInterface"] = "wlp0s20f3"
	//defaults["NetworkInterfaceIPV6Index"] = "2"

	//Paths to search for config file
	configPaths = append(configPaths, defaultPath)

	if fileExists(defaultPath + "config.json") {
		err := os.Remove(defaultPath + "config.json")
		if err != nil {
			return err
		}
	}

	//Calling configuration file
	_, err = ConfigInit()
	if err != nil {
		return err
	}
	return nil
}

func ConfigInit()(*Config,error) {

	curDir := os.Getenv("P2PRC")
	//Setting current directory to default path
	defaultPath = curDir + "/"
	//Paths to search for config file
	configPaths = append(configPaths, defaultPath)

	//Add all possible configurations paths
	for _,v := range configPaths {
		viper.AddConfigPath(v)
	}

	//Read config file
	if err := viper.ReadInConfig(); err != nil {
		// If the error thrown is config file not found
		//Sets default configuration to viper
		for k,v := range defaults {
			viper.SetDefault(k,v)
		}
		viper.SetConfigName(configName)
		viper.SetConfigFile(configFile)
		viper.SetConfigType(configType)

		if err = viper.WriteConfig(); err != nil {
			return nil,err
		}
	}

	// Adds configuration to the struct
	var config Config
	if err := viper.Unmarshal(&config); err != nil {
		return nil,err
	}

	return &config,nil
}
