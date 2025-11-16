//go:build !rust

package rustlibrary

import "fmt"
/*
// For statically link: #cgo LDFLAGS: ./librust_lib.a
// For dynamically link: #cgo LDFLAGS: -L. -lrust_lib
#cgo LDFLAGS: ./libp2prc_loaded.a
*/
import "C"

func TestRust() {
    user := DemoUser{
        name: "chihai",
        age:  28,
    }
    G2RCallImpl{}.demo_log(&user.name, &user.age)
    newName := G2RCallImpl{}.demo_convert_name(&user)
    fmt.Printf("new name: %s", newName)
}
