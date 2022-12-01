package frp

import (
    "github.com/fatedier/frp/client"
    "github.com/fatedier/frp/pkg/config"
)

// Starts FRP client
func StartFRPClient(address string, port int) error {

    cfg := config.GetDefaultClientConf()
    //var cfg config.ClientCommonConf
    var visitorCfgs map[string]config.VisitorConf
    //
    cfg.ServerAddr = address
    cfg.ServerPort = port

    var tcpcnf config.TCPProxyConf
    tcpcnf.LocalIP = "0.0.0.0"
    tcpcnf.LocalPort = 22
    tcpcnf.RemotePort = 3000

    proxyConfs := map[string]config.ProxyConf{
        tcpcnf.ProxyName: &tcpcnf,
    }

    //cfg.ClientConfig = auth.GetDefaultClientConf()
    //cfg.Protocol = "tcp"

    //pxyCfgs, visitorCfgs, _ = config.LoadAllProxyConfsFromIni(cfg.User, nil, cfg.Start)

    cli, err := client.NewService(cfg, proxyConfs, visitorCfgs, "")
    if err != nil {
        return err
    }

    cli.Run()

    return nil
}
