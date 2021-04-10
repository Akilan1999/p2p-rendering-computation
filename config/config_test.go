package config

import (
	"testing"
)

func TestConfigInit(t *testing.T) {
	_,err := ConfigInit()
	if err != nil {
		t.Error(err)
	}
}
