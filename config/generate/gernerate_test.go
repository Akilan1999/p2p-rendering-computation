package generate

import (
	"fmt"
	"github.com/Akilan1999/p2p-rendering-computation/config"
	"testing"
)

type CustomConfig struct {
	Test string
}

// Test case to generate defaults with custom data-structure
func TestSetDefaults(t *testing.T) {
	setDefaults, err := SetDefaults("", true, &CustomConfig{Test: "lol"}, true)
	if err != nil {
		fmt.Println(err)
		t.Fail()
		return
	}

	fmt.Println(setDefaults)

	var c CustomConfig

	_, err = config.ConfigInit(nil, &c)
	if err != nil {
		fmt.Println(err)
		t.Fail()
		return
	}

	fmt.Println(c)

}

// Test case to generate public and private keys
func TestGeneratePublicAndPrivateKeys(t *testing.T) {
	MakeSSHKeyPair("test.pub", "test.prv")
}
