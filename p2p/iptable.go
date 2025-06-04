package p2p

import (
    "bytes"
    "crypto/sha256"
    "encoding/json"
    "fmt"
    "github.com/Akilan1999/p2p-rendering-computation/config"
    "io/ioutil"
    "net"
    "net/http"
    "os"
    "time"
)

// Get IP table Data

type IpAddresses struct {
    IpAddress []IpAddress `json:"ip_address"`
}

type IpAddress struct {
    Name                 string        `json:"Name"`
    MachineUsername      string        `json:"MachineUsername"`
    Ipv4                 string        `json:"IPV4"`
    Ipv6                 string        `json:"IPV6"`
    Latency              time.Duration `json:"Latency"`
    Download             float64       `json:"Download"`
    Upload               float64       `json:"Upload"`
    ServerPort           string        `json:"ServerPort"`
    BareMetalSSHPort     string        `json:"BareMetalSSHPort"`
    NAT                  bool          `json:"NAT"`
    EscapeImplementation string        `json:"EscapeImplementation"`
    ProxyServer          bool          `json:"ProxyServer"`
    UnSafeMode           bool          `json:"UnSafeMode"`
    PublicKey            string        `json:"PublicKey"`
    CustomInformation    string        `json:"CustomInformation"`
    //CustomInformationKey []byte        `json:"CustomInformationKey"`
}

type IP struct {
    Query string
}

var Test = false

// ReadIpTable Read data from Ip tables from json file
func ReadIpTable() (*IpAddresses, error) {
    // Get Path from config
    config, err := config.ConfigInit(nil, nil)
    if err != nil {
        return nil, err
    }
    jsonFile, err := os.Open(config.IPTable)
    // if we os.Open returns an error then handle it
    if err != nil {
        return nil, err
    }

    // defer the closing of our jsonFile so that we can parse it later on
    defer jsonFile.Close()

    // read our opened xmlFile as a byte array.
    byteValue, _ := ioutil.ReadAll(jsonFile)

    // we initialize our Users array
    var ipAddresses IpAddresses

    // we unmarshal our byteArray which contains our
    // jsonFile's content into 'users' which we defined above
    json.Unmarshal(byteValue, &ipAddresses)

    var PublicIP IpAddress

    ipv6, err := GetCurrentIPV6()
    if err != nil {
        return nil, err
    }

    ip, err := CurrentPublicIP()
    if err != nil {
        return nil, err
    }
    PublicIP.Ipv4 = ip
    PublicIP.Ipv6 = ipv6
    PublicIP.ServerPort = config.ServerPort
    PublicIP.Name = config.MachineName
    PublicIP.NAT = config.BehindNAT
    PublicIP.EscapeImplementation = "None"

    // Updates current machine IP address to the IP table
    ipAddresses.IpAddress = append(ipAddresses.IpAddress, PublicIP)

    //before writing to iptable ensures the duplicates are removed
    if err = ipAddresses.RemoveDuplicates(); err != nil {
        return nil, err
    }

    return &ipAddresses, nil
}

// WriteIpTable Write to IP table json file
func (i *IpAddresses) WriteIpTable() error {
    //before writing to iptable ensures the duplicates are removed
    if err := i.RemoveDuplicates(); err != nil {
        return err
    }

    file, err := json.MarshalIndent(i, "", " ")
    if err != nil {
        return err
    }

    // Get Path from config
    config, err := config.ConfigInit(nil, nil)
    if err != nil {
        return err
    }

    err = ioutil.WriteFile(config.IPTable, file, 0644)
    if err != nil {
        return err
    }

    return nil
}

// PrintIpTable Print Ip table data for Cli
func PrintIpTable() error {
    table, err := ReadIpTable()
    //
    if err != nil {
        return err
    }
    //
    //for i := 0; i < len(table.IpAddress); i++ {
    //    fmt.Printf("\nMachine Name: %s\nIP Address: %s\nIPV6: %s\nLatency: %s\nServerPort: %s\nbehindNAT: %s\nEscapeImplementation: %s\n-----------"+
    //        "-----------------\n", table.IpAddress[i].Name, table.IpAddress[i].Ipv4, table.IpAddress[i].Ipv6,
    //        table.IpAddress[i].Latency, table.IpAddress[i].ServerPort, table.IpAddress[i].NAT, table.IpAddress[i].EscapeImplementation)
    //}
    PrettyPrint(table)

    return nil
}

// RemoveDuplicates This is a temporary fix current functions failing to remove
// Duplicate IP addresses from local IP table
func (table *IpAddresses) RemoveDuplicates() error {

    var NoDuplicates IpAddresses
    for i, _ := range table.IpAddress {
        Exists := false
        for k := range NoDuplicates.IpAddress {
            // Statements checked for
            // - duplicate IPV4 addresses [<IPV4>:<Port No>]
            // - duplicate IPV6 addresses [<IPV6>]
            // - Node is behind NAT and no escape implementation provided
            if (NoDuplicates.IpAddress[k].Ipv4 != "" && NoDuplicates.IpAddress[k].Ipv4 == table.IpAddress[i].Ipv4 &&
                NoDuplicates.IpAddress[k].ServerPort == table.IpAddress[i].ServerPort) ||
                (NoDuplicates.IpAddress[k].Ipv6 != "" && NoDuplicates.IpAddress[k].Ipv6 == table.IpAddress[i].Ipv6) {
                Exists = true
                break
            }
        }

        if table.IpAddress[i].NAT && table.IpAddress[i].EscapeImplementation == "None" {
            Exists = true
        }

        if Exists {
            continue
        }
        NoDuplicates.IpAddress = append(NoDuplicates.IpAddress, table.IpAddress[i])
    }

    table.IpAddress = NoDuplicates.IpAddress

    return nil
}

// CurrentPublicIP Get Current Public IP address
func CurrentPublicIP() (string, error) {
    // Get configs
    Config, err := config.ConfigInit(nil, nil)
    if err != nil {
        return "", err
    }
    // If test mode is on then return local address
    if Config.Test {
        return "0.0.0.0", nil
    }

    req, err := http.Get("http://ip-api.com/json/")
    if err != nil {
        return "", err
    }
    defer req.Body.Close()

    body, err := ioutil.ReadAll(req.Body)
    if err != nil {
        return "", err
    }

    var ip IP
    json.Unmarshal(body, &ip)

    return ip.Query, nil

}

// GetCurrentIPV6 gets the current IPV6	address based on the interface
// specified in the config file
func GetCurrentIPV6() (string, error) {
    Config, err := config.ConfigInit(nil, nil)
    if err != nil {
        return "", err
    }

    // Fix in future release
    //byNameInterface, err := net.InterfaceByName(Config.NetworkInterface)
    //if err != nil {
    //	return "",err
    //}
    //addresses, err := byNameInterface.Addrs()
    //if err != nil {
    //	return "",err
    //}
    //if addresses[1].String() == "" {
    //	return "",errors.New("IPV6 address not detected")
    //}
    //IP,_,err := net.ParseCIDR(addresses[Config.NetworkInterfaceIPV6Index].String())
    //if err != nil {
    //	return "",err
    //}

    return Config.IPV6Address, nil
}

// ViewNetworkInterface This function is created to view the network interfaces available
func ViewNetworkInterface() error {
    ifaces, err := net.Interfaces()
    if err != nil {
        return err
    }
    for _, i := range ifaces {
        addrs, err := i.Addrs()
        if err != nil {
            return err
        }
        for index, a := range addrs {
            switch v := a.(type) {
            case *net.IPAddr:
                fmt.Printf("(%v) %v : %s (%s)\n", index, i.Name, v, v.IP.DefaultMask())

            case *net.IPNet:
                fmt.Printf("(%v) %v : %s \n", index, i.Name, v)
            }

        }
    }
    return nil
}

// Ip4or6 Helper function to check if the IP address is IPV4 or
// IPV6 (https://socketloop.com/tutorials/golang-check-if-ip-address-is-version-4-or-6)
func Ip4or6(s string) string {
    for i := 0; i < len(s); i++ {
        switch s[i] {
        case '.':
            return "version 4"
        case ':':
            return "version 6"
        }
    }
    return "version 6"

}

func PrettyPrint(data interface{}) {
    var p []byte
    //    var err := error
    p, err := json.MarshalIndent(data, "", "\t")
    if err != nil {
        fmt.Println(err)
        return
    }
    fmt.Printf("%s \n", p)
}

func GenerateHashSHA256(text string) []byte {
    h := sha256.New()
    h.Write([]byte(text))

    return h.Sum(nil)
}

// ValidateHashSHA256 CustomInformationKey the text and check if the text and
// the hash are the same, if they are
// then return true
// SHA256 is the current hashing algorthm
// used.
func ValidateHashSHA256(text string, Hash []byte) bool {
    h := sha256.New()
    h.Write([]byte(text))

    textHash := h.Sum(nil)
    if bytes.Equal(textHash, Hash) {
        return true
    }
    return false
}
