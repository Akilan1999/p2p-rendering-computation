package config

import (
    "github.com/spf13/viper"
    "os"
)

var (
    //defaultPath    string
    defaults       = map[string]interface{}{}
    configName     = "config"
    configType     = "json"
    configFile     = "config.json"
    configPaths    []string
    defaultEnvName = "P2PRC"
)

type Config struct {
    MachineName              string
    IPTable                  string
    DockerContainers         string
    DefaultDockerFile        string
    SpeedTestFile            string
    IPV6Address              string
    PluginPath               string
    TrackContainersPath      string
    ServerPort               string
    GroupTrackContainersPath string
    FRPServerPort            string
    BehindNAT                string
    //NetworkInterface  string
    //NetworkInterfaceIPV6Index int
}

// GetPathP2PRC Getting P2PRC Directory from environment variable
func GetPathP2PRC(Envname string) (string, error) {
    if Envname != "" {
        err := SetEnvName(Envname)
        if err != nil {
            return "", err
        }
    }
    curDir := os.Getenv(defaultEnvName)
    return curDir + "/", nil
}

// SetEnvName Sets the environment name
// This is to ensure that the Path of your project is detected from
// your environment variable
// This is useful when extending the use case of P2PRC
func SetEnvName(EnvName string) error {
    defaultEnvName = EnvName
    // Handling error to be implemented only if needed
    return nil
}

func GetEnvName() string {
    return defaultEnvName
}

// ConfigInit Pass environment name as an optional parameter
func ConfigInit(defaultsParameter map[string]interface{}, envNameOptional ...string) (*Config, error) {
    if len(envNameOptional) > 0 {
        defaultEnvName = envNameOptional[0]
    }

    //Setting current directory to default path
    defaultPath, err := GetPathP2PRC(defaultEnvName)
    if err != nil {
        return nil, err
    }
    //Paths to search for config file
    configPaths = append(configPaths, defaultPath)

    //Add all possible configurations paths
    for _, v := range configPaths {
        viper.AddConfigPath(v)
    }

    //Read config file
    if err := viper.ReadInConfig(); err != nil {
        // If the error thrown is config file not found
        //Sets default configuration to viper
        for k, v := range defaults {
            viper.SetDefault(k, v)
        }
        viper.SetConfigName(configName)
        viper.SetConfigFile(configFile)
        viper.SetConfigType(configType)

        if err = viper.WriteConfig(); err != nil {
            return nil, err
        }
    }

    // Adds configuration to the struct
    var config Config
    if err := viper.Unmarshal(&config); err != nil {
        return nil, err
    }

    return &config, nil
}
