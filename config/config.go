package config

import(
	"github.com/spf13/viper"
)

var (
	defaults = map[string]interface{}{
		"IPTable": "/etc/p2p-rendering/ip_table.json",
		"DockerFile": "/home/akilan/Documents/p2prendering/p2p-redering-computation/server/docker/containers/docker-ubuntu-sshd/",
		"SpeedTestFile":"/etc/p2p-rendering/50.bin",
	}
	configName = "config.json"
	configType = "json"
	configFile = "config.json"
	configPaths = []string {
		"/etc/p2p-rendering",
		".",
	}
)

type Config struct {
	IPTable string
	DockerFile string
	SpeedTestFile string
}

func ConfigInit()(*Config,error) {
	//Sets default configuration to viper
	for k,v := range defaults {
		viper.SetDefault(k,v)
	}
	viper.SetConfigName(configName)
	viper.SetConfigFile(configFile)
	viper.SetConfigType(configType)
	//Add all possible configurations paths
	for _,v := range configPaths {
		viper.AddConfigPath(v)
	}
	//Read config file
	if err := viper.ReadInConfig(); err != nil {
		// If the error thrown is config file not found
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
