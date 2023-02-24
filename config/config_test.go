package config

import (
    "fmt"
    "github.com/Akilan1999/p2p-rendering-computation/config/generate"
    "os"
    "testing"
)

func TestConfigInit(t *testing.T) {
    _, err := ConfigInit(nil)
    if err != nil {
        t.Error(err)
    }
}

func TestSetDefaults(t *testing.T) {
    _, err := generate.SetDefaults("", false)
    if err != nil {
        t.Error(err)
    }
}

func TestGetCurrentPath(t *testing.T) {
    path, err := generate.GetCurrentPath()
    if err != nil {
        fmt.Println(err)
        t.Error(err)
    }
    fmt.Println(path)
}

func TestGetPathP2PRC(t *testing.T) {
    path, err := GetPathP2PRC("")
    if err != nil {
        fmt.Println(err)
        t.Error(err)
    }
    fmt.Println(path)
}

func TestSetEnvName(t *testing.T) {
    // Create an Env variable TEST with the value "lol"
    err := os.Setenv("TEST", "lol")
    if err != nil {
        fmt.Println(err)
        t.Error(err)
    }
    // Sets the environment variable as the default to read
    // for P2PRC
    err = SetEnvName("TEST")
    if err != nil {
        fmt.Println(err)
        t.Error(err)
    }

    // Checks if the output for the default read is "lol"
    path, err := GetPathP2PRC("")
    if err != nil {
        fmt.Println(err)
        t.Error(err)
    }
    fmt.Println(path)
}
