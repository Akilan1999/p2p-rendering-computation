package plugin

import "testing"

// Test if the dummy plugin added is detected
func TestDetectPlugins(t *testing.T) {
	_, err := DetectPlugins()
	if err != nil {
		t.Fail()
	}
}

func TestRunPlugin(t *testing.T) {
	var testips []*ExecuteIP
	var testip1,testip2 ExecuteIP

	//Test IP 1 configuration
	testip1.IPAddress = "0.0.0.0"
	testip1.SSHPortNo = "41289"

	//Test IP 2 configuration
	testip2.IPAddress = "0.0.0.0"
	testip1.SSHPortNo = "34447"

	testips = append(testips, &testip1)
	testips = append(testips, &testip2)

	RunPlugin("test",testips)
}