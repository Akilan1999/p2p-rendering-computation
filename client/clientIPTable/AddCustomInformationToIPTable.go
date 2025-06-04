package clientIPTable

import (
    "errors"
    "github.com/Akilan1999/p2p-rendering-computation/config"
    "github.com/Akilan1999/p2p-rendering-computation/p2p"
)

func AddCustomInformationToIPTable(text string) error {
    // Get config information
    Config, err := config.ConfigInit(nil, nil)
    if err != nil {
        return err
    }

    // Get IPTable information
    table, err := p2p.ReadIpTable()
    if err != nil {
        return err
    }

    found := false

    for i, _ := range table.IpAddress {
        if table.IpAddress[i].Name == Config.MachineName {
            table.IpAddress[i].CustomInformation = text
            found = true
        }
    }

    if found {
        table.WriteIpTable()
        // update IPTable after modified entry
        UpdateIpTableListClient()
    } else {
        return errors.New("start server with p2prc -s as the server is currently not running")
    }

    return nil
}
