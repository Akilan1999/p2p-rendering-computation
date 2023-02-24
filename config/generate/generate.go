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
func SetDefaults(envName string, forceDefault bool, ConfigUpdate ...*config.Config) (*config.Config, error) {
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

    if len(ConfigUpdate) == 0 {
        //Setting default paths for the config file
        defaults["IPTable"] = defaultPath + "p2p/iptable/ip_table.json"
        defaults["DefaultDockerFile"] = defaultPath + "server/docker/containers/docker-ubuntu-sshd/"
        defaults["DockerContainers"] = defaultPath + "server/docker/containers/"
        defaults["SpeedTestFile"] = defaultPath + "p2p/50.bin"
        defaults["IPV6Address"] = ""
        defaults["PluginPath"] = defaultPath + "plugin/deploy"
        defaults["TrackContainersPath"] = defaultPath + "client/trackcontainers/trackcontainers.json"
        defaults["GroupTrackContainersPath"] = defaultPath + "client/trackcontainers/grouptrackcontainers.json"
        defaults["ServerPort"] = "8088"
        defaults["FRPServerPort"] = "0"
        defaults["BehindNAT"] = "True"
        // Random name generator
        hostname, err := os.Hostname()
        if err != nil {
            return nil, err
        }

        defaults["MachineName"] = hostname
    } else {
        defaults["IPTable"] = ConfigUpdate[0].IPTable
        defaults["DefaultDockerFile"] = ConfigUpdate[0].DefaultDockerFile
        defaults["DockerContainers"] = ConfigUpdate[0].DockerContainers
        defaults["SpeedTestFile"] = ConfigUpdate[0].SpeedTestFile
        defaults["IPV6Address"] = ConfigUpdate[0].IPV6Address
        defaults["PluginPath"] = ConfigUpdate[0].PluginPath
        defaults["TrackContainersPath"] = ConfigUpdate[0].TrackContainersPath
        defaults["GroupTrackContainersPath"] = ConfigUpdate[0].GroupTrackContainersPath
        defaults["ServerPort"] = ConfigUpdate[0].ServerPort
        defaults["FRPServerPort"] = ConfigUpdate[0].FRPServerPort
        defaults["BehindNAT"] = ConfigUpdate[0].BehindNAT
    }

    //defaults["NetworkInterface"] = "wlp0s20f3"
    //defaults["NetworkInterfaceIPV6Index"] = "2"

    //Paths to search for config file
    configPaths = append(configPaths, defaultPath)

    if fileExists(defaultPath + "config.json") && forceDefault {
        err := os.Remove(defaultPath + "config.json")
        if err != nil {
            return nil, err
        }
    }

    //Calling configuration file
    Config, err := config.ConfigInit(defaults, envName)
    if err != nil {
        return nil, err
    }

    err = GenerateFiles()
    if err != nil {
        return nil, err
    }
    return Config, nil
}
