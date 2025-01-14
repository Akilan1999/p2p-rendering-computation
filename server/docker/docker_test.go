package docker

// import (
// 	"testing"
// )
//
// func TestDockerUbuntuSSHDProvided(t *testing.T) {
// 	// Testing by providing default container name
// 	_,err := BuildRunContainer(2,"false","docker-ubuntu-sshd")
//
// 	if err != nil {
// 		t.Error(err)
// 	}
//
// }
//
// func TestDockerDefaultContainer(t *testing.T) {
// 	// Testing by providing without providing default container name
// 	_,err := BuildRunContainer(2,"false","")
//
// 	if err != nil {
// 		t.Error(err)
// 	}
// }
//
// func TestContainerHorovod(t *testing.T) {
// 	// Testing by providing the horovod cpu image
// 	_,err := BuildRunContainer(2,"false","cpuhorovod")
//
// 	if err != nil {
// 		t.Error(err)
// 	}
// }
//
// func TestViewAllContainers(t *testing.T) {
// 	_,err := ViewAllContainers()
//
// 	if err != nil {
// 		t.Error(err)
// 	}
//
// }
