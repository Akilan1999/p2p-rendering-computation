package server

import (
    "context"
    "fmt"

    "github.com/libp2p/go-libp2p"
    lssh "github.com/wanyvic/go-libp2p-ssh"
)

func server() {
    listenAddrs := libp2p.ListenAddrStrings(
        "/ip4/0.0.0.0/tcp/9001",
    )
    host, err := libp2p.New(context.Background(), listenAddrs)
    if err != nil {
        fmt.Println(err)
    }
    fmt.Printf("Your PeerID is :%s\nListen:%s\n", host.ID().String(), host.Addrs())
    /*Your PeerID is :QmZ8zzzFhZAxWHzWecrj6x1r4UH9TnD35f6hBom3TbRGpu
Listen:[/ip4/127.0.0.1/tcp/9000 /ip4/192.168.3.131/tcp/9000 /ip4/192.168.0.133/tcp/9000 /ip4/172.17.0.1/tcp/9000]*/
    lssh.NewSSHService(host)
    select {}   //hold on
}