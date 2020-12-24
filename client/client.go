package client

import (
    "context"
    "fmt"
    "io/ioutil"
    "os"
    "time"

    "github.com/libp2p/go-libp2p"
    "github.com/libp2p/go-libp2p-core/peer"
    ma "github.com/multiformats/go-multiaddr"
    lssh "github.com/wanyvic/go-libp2p-ssh"
    "github.com/wanyvic/ssh"
)

func client() {
    host, err := libp2p.New(context.Background())
    if err != nil {
        fmt.Println(err)
    }
    fmt.Printf("Your PeerID is :%s\nListen:%s\n", host.ID().String(), host.Addrs())
    //pid: /ip4/127.0.0.1/tcp/9000/p2p/QmZ8zzzFhZAxWHzWecrj6x1r4UH9TnD35f6hBom3TbRGpu

    maddr, err := ma.NewMultiaddr(pid)
    if err != nil {
        fmt.Println(err)
    }
    peerinfo, _ := peer.AddrInfoFromP2pAddr(maddr)
    if err := host.Connect(context.Background(), *peerinfo); err != nil {
        fmt.Println(err)
    }

    //auth
    auth := make([]ssh.AuthMethod, 0)
    // password authentication
    auth = append(auth, ssh.Password("xxxx")) //your os password

    // public key authentication
    home := os.Getenv("HOME")

    privateBytes, err := ioutil.ReadFile(home + "/.ssh/id_rsa")
    if err != nil {
        fmt.Println(err)
    }
    Signer, err := ssh.ParsePrivateKey(privateBytes)
    if err != nil {
        fmt.Println(err)
    }
    auth = append(auth, ssh.PublicKeys(Signer))

    //create clientConfig
    clientConfig := &ssh.ClientConfig{
        User:    "wany", // username which you want to login with
        Auth:    auth,
        Timeout: 30 * time.Second,
        HostKeyCallback: func(hostname string, remote ma.Multiaddr, key ssh.PublicKey) error {
            return nil
        },
    }

    clients := lssh.NewSSHClientWithConfig(host, *clientConfig)

    //bind reader writer
    clients.Stdout = os.Stdout
    clients.Stderr = os.Stderr
    clients.Stdin = os.Stdin

    clients.Connect(peerinfo.ID)
}