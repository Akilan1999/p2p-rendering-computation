package plugin

import "testing"

// Test if the dummy plugin added is detected
func TestDetectPlugins(t *testing.T) {
	_, err := DetectPlugins()
	if err != nil {
		t.Fail()
	}
}