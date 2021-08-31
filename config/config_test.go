package config

import (
	"fmt"
	"testing"
)

func TestConfigInit(t *testing.T) {
	_,err := ConfigInit()
	if err != nil {
		t.Error(err)
	}
}

func TestSetDefaults(t *testing.T) {
	err := SetDefaults()
	if err != nil {
		t.Error(err)
	}
}

func TestGetCurrentPath(t *testing.T) {
	path, err := GetCurrentPath()
	if err != nil {
		fmt.Println(err)
		t.Error(err)
	}
	fmt.Println(path)
}

func TestGetPathP2PRC(t *testing.T) {
	path, err := GetPathP2PRC()
	if err != nil {
		fmt.Println(err)
		t.Error(err)
	}
	fmt.Println(path)
}
