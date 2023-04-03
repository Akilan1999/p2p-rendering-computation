package generate

import (
    "github.com/Akilan1999/p2p-rendering-computation/config"
    "os"
)

var (
    defaults       = map[string]interface{}{}
    configPaths    []string
    defaultEnvName = "P2PRC"
)

// GetPathP2PRC Getting P2PRC Directory from environment variable
func GetPathP2PRC() (string, error) {
    curDir := os.Getenv(defaultEnvName)
    return curDir + "/", nil
}

// SetEnvName Sets the environment name
// This is to ensure that the Path of your project is detected from
// your environment variable
// This is useful when extending the use case of P2PRC
func SetEnvName(EnvName string) error {
    if EnvName != "" {
        defaultEnvName = EnvName
    }
    // Handling error to be implemented only if needed
    return nil
}

// GetCurrentPath Getting P2PRC Directory from environment variable
func GetCurrentPath() (string, error) {
    curDir := os.Getenv("PWD")
    return curDir + "/", nil
}

// SetDefaults This function to be called only during a
// make install
func SetDefaults(envName string, forceDefault bool, CustomConfig interface{}, NoBoilerPlate bool, ConfigUpdate ...*config.Config) (*config.Config, error) {
    //Setting current directory to default path
    defaultPath, err := GetCurrentPath()
    if err != nil {
        return nil, err
    }

    // Set Env name
    err = config.SetEnvName(envName)
    if err != nil {
        return nil, err
    }

    ////Creates ip_table.json in the json directory
    //err = Copy("p2p/ip_table.json", "p2p/iptable/ip_table.json")
    //if err != nil {
    //    return err
    //}
    //
    ////Creates a copy of trackcontainers.json in the appropriate directory
    //err = Copy("client/trackcontainers.json", "client/trackcontainers/trackcontainers.json")
    //if err != nil {
    //    return err
    //}
    //
    ////Creates a copy of trackcontainers.json in the appropriate directory
    //err = Copy("client/grouptrackcontainers.json", "client/trackcontainers/grouptrackcontainers.json")
    //if err != nil {
    //    return err
    //}

    var Defaults config.Config

    if len(ConfigUpdate) == 0 {
        //Setting default paths for the config file
        Defaults.IPTable = defaultPath + "p2p/ip_table.json"
        // Defaults.IPTable = defaultPath + "p2p/iptable/ip_table.json"
        Defaults.DefaultDockerFile = defaultPath + "server/docker/containers/docker-ubuntu-sshd/"
        Defaults.DockerContainers = defaultPath + "server/docker/containers/"
        Defaults.SpeedTestFile = defaultPath + "p2p/50.bin"
        Defaults.IPV6Address = ""
        Defaults.PluginPath = defaultPath + "plugin/deploy"
        Defaults.TrackContainersPath = defaultPath + "client/trackcontainers/trackcontainers.json"
        Defaults.GroupTrackContainersPath = defaultPath + "client/trackcontainers/grouptrackcontainers.json"
        Defaults.ServerPort = "8088"
        Defaults.FRPServerPort = "True"
        Defaults.CustomConfig = CustomConfig
        Defaults.BehindNAT = "True"
        // Random name generator
        hostname, err := os.Hostname()
        if err != nil {
            return nil, err
        }

        Defaults.MachineName = hostname
    } else {
        Defaults = *ConfigUpdate[0]
    }

    //defaults["NetworkInterface"] = "wlp0s20f3"
    //defaults["NetworkInterfaceIPV6Index"] = "2"

    //Paths to search for config file
    configPaths = append(configPaths, defaultPath)

    if fileExists(defaultPath+"config.json") && forceDefault {
        err := os.Remove(defaultPath + "config.json")
        if err != nil {
            return nil, err
        }
    }

    // write defaults to the config file
    err = Defaults.WriteConfig()
    if err != nil {
        return nil, err
    }

    //Calling configuration file
    Config, err := config.ConfigInit(defaults, nil, envName)
    if err != nil {
        return nil, err
    }

    if !NoBoilerPlate {
        err = GenerateFiles()
        if err != nil {
            return nil, err
        }
    }

    return Config, nil
}
