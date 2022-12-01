package frp

import (
    "fmt"
    "testing"
    "time"
)

func TestStartFRPServer(t *testing.T) {
    err := StartFRPServer("127.0.0.1", 8808)
    if err != nil {
        fmt.Println(err)
        t.Fail()
    }
}

func TestStartFRPClient(t *testing.T) {
    go StartFRPServer("127.0.0.1", 8808)
    //if err != nil {
    //    fmt.Println(err)
    //    t.Fail()
    //}

    time.Sleep(3 * time.Second)

    err := StartFRPClient("127.0.0.1", 8808)
    if err != nil {
        fmt.Println(err)
        t.Fail()
    }
}
