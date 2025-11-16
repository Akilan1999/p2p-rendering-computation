//go:build !rust

// Package rustlibrary
// This is samle package that mentions to the go program that
// the rust .a file is not linked to the binary. This is to
// always allow p2prc to work with pure go.
package rustlibrary

import "fmt"

func TestRust() {
    fmt.Println("Rust module not built")
}
